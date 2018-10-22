{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Server where

import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Gzip
import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BS
import Servant
import Servant.Auth.Server as Server
import Servant.Auth.Server.SetCookieOrphan ()
import System.IO


type Api =
  (Server.Auth '[Cookie] User :> Protected) :<|>
  Public


type Protected =
  "status" :> Get '[JSON] User


type Public =
  "logout" :> Post '[JSON] (CredHeaders NoContent) :<|>
  "login" :> ReqBody '[JSON] Login :> Post '[JSON] (CredHeaders User) :<|>
  Raw


type CredHeaders a =
  Headers '[ Header "Set-Cookie" SetCookie
           , Header "Set-Cookie" SetCookie
           ] a


api :: Proxy Api
api = Proxy


run :: IO ()
run = do
  key <- generateKey
  let jwtSettings =
        defaultJWTSettings key
      cookieSettings =
        defaultCookieSettings
        { cookieIsSecure = NotSecure
        , cookieSameSite = SameSiteStrict
        }
      context =
         cookieSettings :. jwtSettings :. EmptyContext
  Warp.runSettings settings .
    gzip def { gzipFiles = GzipCompress } .
    serveWithContext api context $
    server cookieSettings jwtSettings


settings :: Warp.Settings
settings =
  Warp.setPort port $
  Warp.setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
  Warp.defaultSettings
  where port = 8080


server :: CookieSettings -> JWTSettings -> Server Api
server cookieSettings jwtSettings =
  protected :<|>
  public cookieSettings jwtSettings


protected :: Server.AuthResult User -> Server Protected
protected authResult =
  case authResult of
    (Server.Authenticated user) ->
      return user
    err ->
      throwAll err401{ errBody = BS.pack $ show err }


public :: CookieSettings -> JWTSettings -> Server Public
public cookieSettings jwtSettings =
  logout cookieSettings :<|>
  checkCredentials cookieSettings jwtSettings :<|>
  Servant.serveDirectoryFileServer "./"


logout :: CookieSettings -> Handler (CredHeaders NoContent)
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
  { username :: String
  , password :: String
  } deriving (Eq, Show, Read, Generic)


instance ToJSON Login
instance FromJSON Login


checkCredentials :: CookieSettings -> JWTSettings -> Login -> Handler (CredHeaders User)
checkCredentials cookieSettings jwtSettings (Login "admin" "admin") = do
   let usr =
         User "Administrator" "hello@tokonoma.com"
   maybeAddCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
   case maybeAddCookies of
     Nothing ->
       throwError err401
     Just addCookies ->
       return $ addCookies usr
checkCredentials _ _ _ =
  throwError err401
