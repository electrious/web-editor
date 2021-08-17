module API where

import Prelude

import Affjax (Error, Request, Response, defaultRequest, printError, request)
import Affjax.RequestBody (RequestBody, formData)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormat, ignore, json)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array as Array
import Data.Compactable (separate)
import Data.Default (class Default)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff, error, killFiber, launchAff_, message, runAff)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Extra (performEvent)
import Type.Proxy (Proxy(..))
import Web.XHR.FormData (FormData)


data APIError = NetworkError Error
              | DataError JsonDecodeError
              | AffError Aff.Error
              | StubError

showAPIError :: APIError -> String
showAPIError (NetworkError e) = "Network error: " <> printError e
showAPIError (DataError e)    = "Data error: "    <> printJsonDecodeError e
showAPIError (AffError e)     = "Aff error: "     <> message e
showAPIError StubError        = "Stub error"

-- | convert an Aff action into a FRP Event
affEvt :: forall a. Aff a -> Event (Either APIError a)
affEvt aff = makeEvent \k -> do
    f <- runAff (mapLeft AffError >>> k) aff
    pure $ launchAff_ $ killFiber (error "kill Aff fiber") f


requestEvt :: forall a. Request a -> Event (Either APIError (Response a))
requestEvt req = join <<< map (mapLeft NetworkError) <$> affEvt (request req)

decodeResp :: forall res. DecodeJson res => Response Json -> Either APIError res
decodeResp resp = mapLeft DataError $ decodeJson resp.body

-- | tap an action on an event
tap :: forall a. (a -> Effect Unit) -> Event a -> Event a
tap f evt = makeEvent \k -> subscribe evt \e -> f e *> k e

mapLeft :: forall a b c. (a -> c) -> Either a b -> Either c b
mapLeft f (Left e)  = Left (f e)
mapLeft _ (Right a) = Right a

-- | filter the Right value of Either in an event
onlyRight :: forall a e. Event (Either e a) -> Event a
onlyRight = f <<< separate
    where f v = v.right

newtype APIConfig = APIConfig {
    auth       :: Maybe String,
    xUserId    :: Maybe Int,
    xCompanyId :: Maybe Int,
    baseUrl    :: String
}

derive instance Newtype APIConfig _
derive instance Generic APIConfig _

instance defaultAPIConfig :: Default APIConfig where
    def = APIConfig {
        auth       : Nothing,
        xUserId    : Nothing,
        xCompanyId : Nothing,
        baseUrl    : ""
    }
_auth :: Lens' APIConfig (Maybe String)
_auth = _Newtype <<< prop (Proxy :: Proxy "auth")

_xUserId :: Lens' APIConfig (Maybe Int)
_xUserId = _Newtype <<< prop (Proxy :: Proxy "xUserId")

_xCompanyId :: Lens' APIConfig (Maybe Int)
_xCompanyId = _Newtype <<< prop (Proxy :: Proxy "xCompanyId")

_baseUrl :: Lens' APIConfig String
_baseUrl = _Newtype <<< prop (Proxy :: Proxy "baseUrl")

newtype API a = API (ReaderT APIConfig Effect a)

derive newtype instance Functor API
derive newtype instance Applicative API
derive newtype instance Apply API
derive newtype instance Bind API
derive newtype instance Monad API
derive newtype instance MonadAsk APIConfig API
derive newtype instance MonadEffect API

runAPI :: forall a. API a -> APIConfig -> Effect a
runAPI (API a) = runReaderT a


performAPIEvent :: forall a. Event (API a) -> API (Event a)
performAPIEvent e = do
    cfg <- ask
    pure $ performEvent $ flip runAPI cfg <$> e


logLeft :: forall a. Either APIError a -> Effect Unit
logLeft (Left e) = log $ showAPIError e
logLeft _        = pure unit

mkRequest :: forall a. Method -> String -> RequestBody -> ResponseFormat a -> API (Request a)
mkRequest method url body format = do
    cfg <- ask
    let authHeader    = fromMaybe [] $ Array.singleton <<< RequestHeader "Authorization"         <$> cfg ^. _auth
        userHeader    = fromMaybe [] $ Array.singleton <<< RequestHeader "x-user-id" <<< show    <$> cfg ^. _xUserId
        companyHeader = fromMaybe [] $ Array.singleton <<< RequestHeader "x-company-id" <<< show <$> cfg ^. _xCompanyId

    pure $ defaultRequest { method         = Left method,
                            headers        = authHeader <> userHeader <> companyHeader,
                            url            = (cfg ^. _baseUrl) <> url,
                            content        = Just body,
                            responseFormat = format
                          }

-- | call an API and get the result Event
callAPI :: forall req res. EncodeJson req => DecodeJson res => Method -> String -> req -> API (Event (Either APIError res))
callAPI m url req = mkRequest m url (RB.json $ encodeJson req) json >>= requestEvt >>> map (join <<< map decodeResp) >>> pure

-- | call an API, log any errors to console and return only valid result
callAPI' :: forall req res. EncodeJson req => DecodeJson res => Method -> String -> req -> API (Event res)
callAPI' m url req = (tap logLeft >>> onlyRight) <$> callAPI m url req

callAPI_ :: forall req. EncodeJson req => Method -> String -> req -> API (Event (Either APIError Unit))
callAPI_ m url req = mkRequest m url (RB.json $ encodeJson req) ignore >>= requestEvt >>> map (map _.body) >>> pure

-- | call an API, log any errors to console and return only valid result
callAPI_' :: forall req. EncodeJson req => Method -> String -> req -> API (Event Unit)
callAPI_' m url req = (tap logLeft >>> onlyRight) <$> callAPI_ m url req

-- | call an API with form data request
formAPI :: forall res. DecodeJson res => Method -> String -> FormData -> API (Event (Either APIError res))
formAPI m url req = mkRequest m url (formData req) json >>= requestEvt >>> map (join <<< map decodeResp) >>> pure
