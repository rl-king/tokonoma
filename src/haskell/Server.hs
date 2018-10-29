{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Server where

import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Data.Aeson (FromJSON, ToJSON, toEncoding, defaultOptions, encode, genericToEncoding)
import Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Network.HTTP.Types (status200)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai (Middleware, responseLBS)
import Network.Wai.Application.Static
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Servant
import Servant.Auth.Server as Server
import Servant.Multipart
import System.IO
import qualified System.Log.FastLogger as Log

import qualified Database as DB
import Database (Database, Resource(..), NewFile(..))


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
  "resources" :> ReqBody '[JSON] DB.NewResource :> PostCreated '[JSON] NoContent :<|>
  "resources" :> Get '[JSON] [Resource] :<|>
  "resources" :> Capture "resourceid" Int :> DeleteNoContent '[JSON] NoContent :<|>
  "file" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] [Text]


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


-- RUN


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


-- PROTECTED


protected :: Server.AuthResult User -> ServerT Protected AppM
protected authResult =
  case authResult of
    (Server.Authenticated user) ->
      return user :<|>
      addResource :<|>
      allResources :<|>
      deleteResource :<|>
      fileUpload
    _ ->
      throwAll err401


addResource :: DB.NewResource -> AppM NoContent
addResource new@(DB.NewResource title _)= do
  -- log
  logset <- asks logger
  currentTime <- liftIO getCurrentTime
  posix <- liftIO $ fromInteger . round <$> getPOSIXTime
  let msg = LogMessage ("Adding: " <> title) currentTime
  liftIO $ Log.pushLogStrLn logset $ Log.toLogStr msg
  -- insert
  State{database = db} <- ask
  liftIO $ atomically $ modifyTVar db (DB.insert new posix)
  return NoContent


deleteResource :: Int -> AppM NoContent
deleteResource uid = do
  -- log
  logset <- asks logger
  currentTime <- liftIO getCurrentTime
  let msg = LogMessage ("Removing: " <> Text.pack (show uid)) currentTime
  liftIO $ Log.pushLogStrLn logset $ Log.toLogStr msg
  -- delete
  State{database = db} <- ask
  liftIO $ atomically $ modifyTVar db (DB.delete uid)
  return NoContent


allResources :: AppM [Resource]
allResources = do
  State{database = db} <- ask
  liftIO $ fmap DB.all . atomically $ readTVar db


-- PUBLIC


public :: CookieSettings -> JWTSettings -> ServerT Public AppM
public cookieSettings jwtSettings =
  logout cookieSettings :<|>
  login cookieSettings jwtSettings :<|>
  Servant.serveDirectoryWith ((defaultWebAppSettings "./static") { ss404Handler = Just indexHtml })


indexHtml :: Application
indexHtml _ respond = do
  file <- liftIO $ LBS.readFile "static/index.html"
  respond $ responseLBS status200 [] file


logout :: CookieSettings -> AppM (CredHeaders NoContent)
logout cookieSettings =
  return $ clearSession cookieSettings NoContent


fileUpload :: MultipartData Mem -> AppM [Text]
fileUpload multipartData = do
  -- log
  logset <- asks logger
  currentTime <- liftIO getCurrentTime
  posix <- liftIO $ fromInteger . round <$> getPOSIXTime
  let
    msg = LogMessage ("Adding: " <> Text.pack (show files')) currentTime
  liftIO $ Log.pushLogStrLn logset $ Log.toLogStr msg
  -- write to disk
  liftIO $ forM_ files'
    (\file -> LBS.writeFile ("files/" ++ Text.unpack (fdFileName file)) (fdPayload file))
  return $ fmap (\f -> Text.pack ("/files/" ++ Text.unpack (fdFileName f))) files'
    where files' = files multipartData


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
