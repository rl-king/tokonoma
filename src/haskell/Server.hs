{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Server where

import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Data.Aeson (FromJSON, ToJSON, toEncoding, defaultOptions, encode, genericToEncoding)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai (Middleware)
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Servant
import Servant.Auth.Server as Server
import System.IO
import qualified System.Log.FastLogger as Log

import qualified Database as DB
import Database (Database, Resource(..))


data State =
  State
  { database :: TVar Database
  , adminUsername :: Text
  , adminPassword :: Text
  , logger :: Log.LoggerSet
  }

data LogMessage =
  LogMessage
  { message :: !Text
  , timestamp :: !UTCTime
  } deriving (Eq, Show, Generic)


instance FromJSON LogMessage
instance ToJSON LogMessage
  where toEncoding = genericToEncoding defaultOptions


instance Log.ToLogStr LogMessage
  where toLogStr = Log.toLogStr . encode

type AppM =
  ReaderT State Handler


type Api =
  (Server.Auth '[Cookie] User :> Protected) :<|>
  Public


type Protected =
  "status" :> Get '[JSON] User :<|>
  "resources" :> ReqBody '[JSON] Text :> PostCreated '[JSON] NoContent :<|>
  "resources" :> Get '[JSON] [Resource] :<|>
  "resources" :> Capture "resourceid" Int :> DeleteNoContent '[JSON] NoContent


type Public =
  "logout" :> Post '[JSON] (CredHeaders NoContent) :<|>
  "login" :> ReqBody '[JSON] LoginCredentials :> Post '[JSON] (CredHeaders User) :<|>
  Raw


type CredHeaders a =
  Headers '[ Header "Set-Cookie" SetCookie
           , Header "Set-Cookie" SetCookie
           ] a


api :: Proxy Api
api =
  Proxy


hoistContext :: Proxy '[CookieSettings, JWTSettings]
hoistContext =
  Proxy


-- jsonRequestLogger :: IO Middleware
-- jsonRequestLogger =
--   mkRequestLogger $
--   def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }


run :: IO ()
run = do
  key <- generateKey
  initData <- atomically $ newTVar DB.init

  -- warpLogger <- jsonRequestLogger
  appLogger <- Log.newStdoutLoggerSet Log.defaultBufSize
  currentTime <- getCurrentTime

  let msg = LogMessage "Liftoff at " currentTime
  Log.pushLogStrLn appLogger (Log.toLogStr msg) >> Log.flushLogStr appLogger

  let jwtSettings =
        defaultJWTSettings key
      cookieSettings =
        defaultCookieSettings
        { cookieIsSecure = NotSecure
        , cookieSameSite = SameSiteStrict
        }
      context =
         cookieSettings :. jwtSettings :. EmptyContext
      state =
        flip runReaderT (State initData "admin" "admin" appLogger)
  Warp.runSettings settings .
    gzip def { gzipFiles = GzipCompress } .
    -- warpLogger $
    serveWithContext api context $
    hoistServerWithContext api hoistContext state $
    server cookieSettings jwtSettings


settings :: Warp.Settings
settings =
  Warp.setPort port $
  Warp.setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
  Warp.defaultSettings
  where port = 8080


server :: CookieSettings -> JWTSettings -> ServerT Api AppM
server cookieSettings jwtSettings =
  protected :<|>
  public cookieSettings jwtSettings


-- ROUTES


protected :: Server.AuthResult User -> ServerT Protected AppM
protected authResult =
  case authResult of
    (Server.Authenticated user) ->
      return user :<|>
      addResource :<|>
      allResources :<|>
      deleteResource
    _ ->
      throwAll err401


addResource :: Text -> AppM NoContent
addResource title = do
  -- log
  logset <- asks logger
  currentTime <- liftIO getCurrentTime
  let msg = LogMessage ("Adding: " <> title) currentTime
  liftIO $ Log.pushLogStrLn logset $ Log.toLogStr msg
  -- insert
  State{database = db} <- ask
  liftIO $ atomically $ modifyTVar db (DB.insert title)
  return NoContent


deleteResource :: Int -> AppM NoContent
deleteResource uid = do
  -- log
  logset <- asks logger
  currentTime <- liftIO getCurrentTime
  let msg = LogMessage ("Removing: " <> (Text.pack $ show uid)) currentTime
  liftIO $ Log.pushLogStrLn logset $ Log.toLogStr msg
  -- insert
  State{database = db} <- ask
  liftIO $ atomically $ modifyTVar db (DB.delete uid)
  return NoContent


allResources :: AppM [Resource]
allResources = do
  State{database = db} <- ask
  liftIO $ fmap DB.all . atomically $ readTVar db


public :: CookieSettings -> JWTSettings -> ServerT Public AppM
public cookieSettings jwtSettings =
  logout cookieSettings :<|>
  login cookieSettings jwtSettings :<|>
  Servant.serveDirectoryFileServer "./"


logout :: CookieSettings -> AppM (CredHeaders NoContent)
logout cookieSettings =
  return $ clearSession cookieSettings NoContent


-- USER


data User =
  User
  { name :: String
  , email :: String
  } deriving (Eq, Show, Read, Generic)


instance ToJSON User
instance FromJSON User


instance ToJWT User
instance FromJWT User


-- LOGIN


data LoginCredentials =
  LoginCredentials
  { username :: Text
  , password :: Text
  } deriving (Eq, Show, Read, Generic)


instance ToJSON LoginCredentials
instance FromJSON LoginCredentials


login :: CookieSettings -> JWTSettings -> LoginCredentials -> AppM (CredHeaders User)
login cookieSettings jwtSettings credentials = do
  state <- ask
  case validateLogin state credentials of
    Nothing ->
      throwError err401
    Just user -> do
      maybeAddCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
      case maybeAddCookies of
        Nothing ->
          throwError err401
        Just addCookies ->
          return $ addCookies user


validateLogin :: State -> LoginCredentials -> Maybe User
validateLogin state (LoginCredentials u p ) =
  if (u == adminUsername state) && (p == adminPassword state) then
    Just $ User "Administrator" "hello@tokonoma.com"
  else
    Nothing
