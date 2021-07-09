module API where

import Prelude

import Axios (genericAxios)
import Axios.Config (baseUrl, headers, method)
import Axios.Types (Header(..), Method)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Array as Array
import Data.Compactable (separate)
import Data.Default (class Default)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, error, killFiber, launchAff_, runAff)
import Effect.Class.Console (errorShow)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Extra (performEvent)
import Foreign.Generic (class Decode, class Encode, F, ForeignError(..), defaultOptions, genericDecode)

-- | convert an Aff action into a FRP Event
affEvt :: forall a. Aff a -> Event (Either Error a)
affEvt aff = makeEvent \k -> do
    f <- runAff k aff
    pure $ launchAff_ $ killFiber (error "kill Aff fiber") f

-- | tap an action on an event
tap :: forall a. (a -> Effect Unit) -> Event a -> Event a
tap f evt = makeEvent \k -> subscribe evt \e -> f e *> k e

-- | filter the Right value of Either in an event
onlyRight :: forall a e. Event (Either e a) -> Event a
onlyRight = f <<< separate
    where f v = v.right

newtype APIConfig = APIConfig {
    auth    :: Maybe String,
    xUserId :: Maybe Int,
    baseUrl :: String
}

derive instance newtypeAPIConfig :: Newtype APIConfig _
derive instance genericAPIConfig :: Generic APIConfig _
instance decodeAPIConfig :: Decode APIConfig where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

instance defaultAPIConfig :: Default APIConfig where
    def = APIConfig {
        auth    : Nothing,
        xUserId : Nothing,
        baseUrl : ""
    }
_auth :: Lens' APIConfig (Maybe String)
_auth = _Newtype <<< prop (SProxy :: SProxy "auth")

_xUserId :: Lens' APIConfig (Maybe Int)
_xUserId = _Newtype <<< prop (SProxy :: SProxy "xUserId")

_baseUrl :: Lens' APIConfig String
_baseUrl = _Newtype <<< prop (SProxy :: SProxy "baseUrl")

newtype API a = API (ReaderT APIConfig Effect a)

derive newtype instance functorAPI :: Functor API
derive newtype instance applicativeAPI :: Applicative API
derive newtype instance applyAPI :: Apply API
derive newtype instance bindAPI :: Bind API
derive newtype instance monadAPI :: Monad API
derive newtype instance monadAsKAPI :: MonadAsk APIConfig API

foreign import getErrorMessage :: Error -> String

runAPI :: forall a. API a -> APIConfig -> Effect a
runAPI (API a) = runReaderT a


performAPIEvent :: forall a. Event (API a) -> API (Event a)
performAPIEvent e = do
    cfg <- ask
    pure $ performEvent $ flip runAPI cfg <$> e


data APIDataType = JSON
                 | Form

contentType :: APIDataType -> Header
contentType JSON = Header "Content-Type" "application/json"
contentType Form = Header "Content-Type" "multipart/form-data"


apiAction :: forall req res. Encode req => Decode res => Method -> String -> APIDataType -> req -> API (Aff (Either Error res))
apiAction m url dt req = do
    cfg <- ask
    let defHeaders = [contentType dt]
        authHeader = fromMaybe [] $ Array.singleton <<< Header "Authorization" <$> cfg ^. _auth
        userHeader = fromMaybe [] $ Array.singleton <<< Header "x-user-id" <<< show <$> cfg ^. _xUserId
    
    pure $ genericAxios url [method m
                           , headers (defHeaders <> authHeader <> userHeader)
                           , baseUrl $ cfg ^. _baseUrl] req

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
