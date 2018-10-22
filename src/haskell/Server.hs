{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Server where

import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Gzip
import Servant
import Servant.Auth.Server as Server
import Servant.Auth.Server.SetCookieOrphan ()
import System.IO


data State = State
  { resources :: TVar [Resource]
  , adminUsername :: Text
  , adminPassword :: Text
  }


type AppM =
  ReaderT State Handler


type Api =
  (Server.Auth '[Cookie] User :> Protected) :<|>
  Public


type Protected =
  "status" :> Get '[JSON] User :<|>
  "resources" :> ReqBody '[JSON] Resource :> PostCreated '[JSON] Resource


type Public =
  "logout" :> Post '[JSON] (CredHeaders NoContent) :<|>
  "login" :> ReqBody '[JSON] Login :> Post '[JSON] (CredHeaders User) :<|>
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


run :: IO ()
run = do
  key <- generateKey
  initData <- atomically $ newTVar []
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
        (flip runReaderT (State initData "admin" "admin"))
  Warp.runSettings settings .
    gzip def { gzipFiles = GzipCompress } .
    serveWithContext api context $
    hoistServerWithContext api hoistContext state $
    server cookieSettings jwtSettings


settings :: Warp.Settings
settings =
  Warp.setPort port $
  Warp.setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
  Warp.defaultSettings
  where port = 8080


server :: CookieSettings -> JWTSettings -> ServerT Api AppM
server cookieSettings jwtSettings =
  protected :<|>
  public cookieSettings jwtSettings


protected :: Server.AuthResult User -> ServerT Protected AppM
protected authResult =
  case authResult of
    (Server.Authenticated user) ->
      return user :<|>
      (\rsc -> return rsc)
    _ ->
      throwAll err401


public :: CookieSettings -> JWTSettings -> ServerT Public AppM
public cookieSettings jwtSettings =
  logout cookieSettings :<|>
  checkCredentials cookieSettings jwtSettings :<|>
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


data Login =
  Login
  { username :: Text
  , password :: Text
  } deriving (Eq, Show, Read, Generic)


instance ToJSON Login
instance FromJSON Login


checkCredentials :: CookieSettings -> JWTSettings -> Login -> AppM (CredHeaders User)
checkCredentials cookieSettings jwtSettings login = do
   state <- ask
   case validateLogin state login of
     Nothing ->
       throwError err401
     Just user -> do
       maybeAddCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
       case maybeAddCookies of
         Nothing ->
           throwError err401
         Just addCookies ->
           return $ addCookies user


validateLogin :: State -> Login -> Maybe User
validateLogin state (Login u p ) =
  if (u == adminUsername state) && (p == adminPassword state) then
    Just $ User "Administrator" "hello@tokonoma.com"
  else
    Nothing


-- RESOURCE


data Resource =
  Resource
  { _id :: Int
  , _title :: Text
  } deriving (Eq, Show, Read, Generic)


instance ToJSON Resource
instance FromJSON Resource
