module API where

import Prelude

import Affjax (Error(..), Request, Response, defaultRequest, request)
import Affjax.RequestHeader (RequestHeader(..))
import Axios.Types (Method)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJsonWith)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Types.Generic (defaultEncoding)
import Data.Array as Array
import Data.Compactable (separate)
import Data.Default (class Default)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff, error, killFiber, launchAff_, runAff)
import Effect.Aff as Aff
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Extra (performEvent)
import Type.Prelude (Proxy(..))
import Web.HTML.HTMLMetaElement (content)

-- | convert an Aff action into a FRP Event
affEvt :: forall a. Aff a -> Event (Either Aff.Error a)
affEvt aff = makeEvent \k -> do
    f <- runAff k aff
    pure $ launchAff_ $ killFiber (error "kill Aff fiber") f


requestEvt :: forall a. Request a -> Event (Either Error (Response a))
requestEvt req = join <<< mapLeft XHROtherError <$> affEvt (request req)

-- | tap an action on an event
tap :: forall a. (a -> Effect Unit) -> Event a -> Event a
tap f evt = makeEvent \k -> subscribe evt \e -> f e *> k e

-- | filter the Right value of Either in an event
onlyRight :: forall a e. Event (Either e a) -> Event a
onlyRight = f <<< separate
    where f v = v.right

mapLeft :: forall a b c. (a -> c) -> Either a b -> Either c b
mapLeft f (Left v) = Left (f v)
mapLeft f (Right v) = Right v

newtype APIConfig = APIConfig {
    auth    :: Maybe String,
    xUserId :: Maybe Int,
    baseUrl :: String
}

derive instance Newtype APIConfig _
derive instance Generic APIConfig _
instance DecodeJson APIConfig where
    decodeJson = genericDecodeJsonWith (defaultEncoding { unwrapSingleArguments = true })

instance defaultAPIConfig :: Default APIConfig where
    def = APIConfig {
        auth    : Nothing,
        xUserId : Nothing,
        baseUrl : ""
    }
_auth :: Lens' APIConfig (Maybe String)
_auth = _Newtype <<< prop (Proxy :: Proxy "auth")

_xUserId :: Lens' APIConfig (Maybe Int)
_xUserId = _Newtype <<< prop (Proxy :: Proxy "xUserId")

_baseUrl :: Lens' APIConfig String
_baseUrl = _Newtype <<< prop (Proxy :: Proxy "baseUrl")

newtype API a = API (ReaderT APIConfig Effect a)

derive newtype instance Functor API
derive newtype instance Applicative API
derive newtype instance Apply API
derive newtype instance Bind API
derive newtype instance Monad API
derive newtype instance MonadAsk APIConfig API

foreign import getErrorMessage :: Error -> String

runAPI :: forall a. API a -> APIConfig -> Effect a
runAPI (API a) = runReaderT a


performAPIEvent :: forall a. Event (API a) -> API (Event a)
performAPIEvent e = do
    cfg <- ask
    pure $ performEvent $ flip runAPI cfg <$> e


data APIDataType = JSON
                 | Form


contentType :: APIDataType -> RequestHeader
contentType JSON = ContentType "application/json"
contentType Form = ContentType "multipart/form-data"


mkRequest :: forall req. EncodeJson req => Method -> String -> APIDataType -> req -> API (RequestHeader req)
mkRequest m url dt req = do
    cfg <- ask
    let defHeaders = [contentType dt]
        authHeader = fromMaybe [] $ Array.singleton <<< RequestHeader "Authorization" <$> cfg ^. _auth
        userHeader = fromMaybe [] $ Array.singleton <<< RequestHeader "x-user-id" <<< show <$> cfg ^. _xUserId

    pure $ defaultRequest { method  = m,
                            headers = defHeaders <> authHeader <> userHeader,
                            url     = (cfg ^. _baseUrl) <> url,
                            content = encodeJson req
                          }

-- | call an API and get the result Event
callAPI :: forall req res. Encode req => Decode res => Method -> String -> req -> API (Event (F res))
callAPI m url req = apiAction m url JSON req >>= affEvt >>> map (join >>> toF) >>> pure
    where toF (Left e)  = throwError $ singleton $ ForeignError $ getErrorMessage e
          toF (Right v) = pure v

-- | call an API, log any errors to console and return only valid result
callAPI' :: forall req res. Encode req => Decode res => Method -> String -> req -> API (Event res)
callAPI' m url req = (map runExcept >>> tap logLeft >>> onlyRight) <$> callAPI m url req
    where logLeft (Left e) = errorShow e
          logLeft _        = pure unit


-- | call an API with form data request
formAPI :: forall req res. Encode req => Decode res => Method -> String -> req -> API (Event (F res))
formAPI m url req = apiAction m url Form req >>= affEvt >>> map (join >>> toF) >>> pure
    where toF (Left e)  = throwError $ singleton $ ForeignError $ getErrorMessage e
          toF (Right v) = pure v

-- | call a form API, log any errors to console and return only valid result
formAPI' :: forall req res. Encode req => Decode res => Method -> String -> req -> API (Event res)
formAPI' m url req = (map runExcept >>> tap logLeft >>> onlyRight) <$> formAPI m url req
    where logLeft (Left e) = errorShow e
          logLeft _        = pure unit
