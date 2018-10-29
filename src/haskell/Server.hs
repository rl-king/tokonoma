{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Server where

import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Network.HTTP.Types (status200)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai (responseLBS)
import Network.Wai.Application.Static
import Network.Wai.Middleware.Gzip
import Servant
import Servant.Auth.Server as Server
import Servant.Multipart
import System.IO
import qualified System.Log.FastLogger as Log

import qualified Database as DB
import Database (Database, Resource(..), FileInfo(..))
import Server.Log


-- API


type Api =
  Server.Auth '[Cookie] Identity :> Private :<|>
  Public


type Private =
  "status" :> Get '[JSON] Identity :<|>
  "resources" :> ReqBody '[JSON] DB.NewResource :> PostCreated '[JSON] NoContent :<|>
  "resources" :> Get '[JSON] [Resource] :<|>
  "resources" :> Capture "resourceid" Int :> DeleteNoContent '[JSON] NoContent :<|>
  "file" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] [FileInfo]


type Public =
  "logout" :> Post '[JSON] (CredHeaders NoContent) :<|>
  "login" :> ReqBody '[JSON] LoginCredentials :> Post '[JSON] (CredHeaders Identity) :<|>
  Raw


type CredHeaders a =
  Headers
    '[ Header "Set-Cookie" SetCookie
     , Header "Set-Cookie" SetCookie
     ] a


-- PROXY


api :: Proxy Api
api =
  Proxy


hoistContext :: Proxy '[CookieSettings, JWTSettings]
hoistContext =
  Proxy


-- STATE


data State =
  State
  { database :: TVar Database
  , logger :: Log.LoggerSet
  }


type AppM =
  ReaderT State Handler


-- RUN


run :: IO ()
run = do
  jwtKey <- generateKey
  initData <- atomically $ newTVar DB.init

  -- warpLogger <- jsonRequestLogger
  appLogger <- Log.newStdoutLoggerSet Log.defaultBufSize
  currentTime <- getCurrentTime

  let msg = LogMessage "Liftoff at " currentTime
  Log.pushLogStrLn appLogger (Log.toLogStr msg) >> Log.flushLogStr appLogger

  let jwtSettings =
        defaultJWTSettings jwtKey

      cookieSettings =
        defaultCookieSettings
        { cookieIsSecure = NotSecure
        , cookieSameSite = SameSiteStrict
        }

      context =
         cookieSettings :. jwtSettings :. EmptyContext

      state =
        flip runReaderT (State initData appLogger)

  Warp.runSettings warpSettings .
    gzip def { gzipFiles = GzipCompress } .
    -- warpLogger $
    serveWithContext api context $
    hoistServerWithContext api hoistContext state $
    handlers cookieSettings jwtSettings


warpSettings :: Warp.Settings
warpSettings =
  Warp.setPort port $
  Warp.setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
  Warp.defaultSettings
  where port = 8080


handlers :: CookieSettings -> JWTSettings -> ServerT Api AppM
handlers cookieSettings jwtSettings =
  private :<|>
  public cookieSettings jwtSettings


-- PRIVATE


private :: Server.AuthResult Identity -> ServerT Private AppM
private authResult =
  case authResult of
    Server.Authenticated user ->
      return user :<|>
      handleAddResource :<|>
      handleAllResources :<|>
      handleDeleteResource :<|>
      handleFileUpload
    _ ->
      throwAll err401


handleAddResource :: DB.NewResource -> AppM NoContent
handleAddResource new@(DB.NewResource title _ _)= do
  posix <- liftIO $ fromInteger . round <$> getPOSIXTime
  State{database = db} <- ask
  liftIO $ atomically $ modifyTVar db (DB.insert new posix)
  log' ("Adding: " <> title)
  return NoContent


handleDeleteResource :: Int -> AppM NoContent
handleDeleteResource uid = do
  State{database = db} <- ask
  liftIO $ atomically $ modifyTVar db (DB.delete uid)
  log' ("Removing: " <> Text.pack (show uid))
  return NoContent


handleAllResources :: AppM [Resource]
handleAllResources = do
  State{database = db} <- ask
  liftIO $ fmap DB.all . atomically $ readTVar db


-- PUBLIC


public :: CookieSettings -> JWTSettings -> ServerT Public AppM
public cookieSettings jwtSettings =
  handleLogout cookieSettings :<|>
  handleLogin cookieSettings jwtSettings :<|>
  Servant.serveDirectoryWith ((defaultWebAppSettings "./static")
                              { ss404Handler = Just handleSpa })


handleSpa :: Application
handleSpa _ respond = do
  file <- liftIO $ LBS.readFile "static/index.html"
  respond $ responseLBS status200 [] file


handleLogout :: CookieSettings -> AppM (CredHeaders NoContent)
handleLogout cookieSettings =
  return $ clearSession cookieSettings NoContent


handleFileUpload :: MultipartData Mem -> AppM [FileInfo]
handleFileUpload multipartData = do
  liftIO $ forM_ files'
    (\file -> LBS.writeFile ("static/files/" ++ Text.unpack (fdFileName file)) (fdPayload file))
  log' ("Adding: " <> Text.pack (show $ fdFileName <$> files'))
  return $ fileUploadResponse files'
    where files' = files multipartData


fileUploadResponse :: [FileData a] -> [FileInfo]
fileUploadResponse =
  let
    toFileData file =
      FileInfo (fdFileName file) $
      Text.pack ("/files/" ++ Text.unpack (fdFileName file))
  in
  fmap toFileData


handleLogin :: CookieSettings -> JWTSettings -> LoginCredentials -> AppM (CredHeaders Identity)
handleLogin cookieSettings jwtSettings credentials =
  case validateLogin credentials of
    Nothing ->
      throwError err401
    Just user -> do
      maybeAddCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
      case maybeAddCookies of
        Nothing ->
          throwError err401
        Just addCookies ->
          return $ addCookies user


validateLogin :: LoginCredentials -> Maybe Identity
validateLogin (LoginCredentials u p) =
  if (u == "admin") && (p == "admin") then
    Just $ Identity "Administrator" "hello@tokonoma.com"
  else
    Nothing


-- USER


data Identity =
  Identity
  { name :: String
  , email :: String
  } deriving (Eq, Show, Read, Generic)


instance ToJSON Identity
instance FromJSON Identity


instance ToJWT Identity
instance FromJWT Identity


-- LOGIN


data LoginCredentials =
  LoginCredentials
  { username :: Text
  , password :: Text
  } deriving (Eq, Show, Read, Generic)


instance ToJSON LoginCredentials
instance FromJSON LoginCredentials


-- LOG


log' :: Text -> AppM ()
log' msg = do
  currentTime <- liftIO getCurrentTime
  logset <- asks logger
  liftIO $ Log.pushLogStrLn logset $
    Log.toLogStr (LogMessage msg currentTime)
