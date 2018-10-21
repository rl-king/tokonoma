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
import Debug.Trace


type Api =
  (Server.Auth '[Cookie] User :> Protected) :<|>
  Public


type Protected =
  "name" :> Get '[JSON] String :<|>
  "email" :> Get '[JSON] String


type Public =
  "login" :> ReqBody '[JSON] Login :> Post '[JSON] CredHeaders :<|>
  Raw


type CredHeaders =
  Headers '[ Header "Set-Cookie" SetCookie
           , Header "Set-Cookie" SetCookie
           ] User


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
server cs jwts =
  protected :<|>
  public cs jwts


protected :: Server.AuthResult User -> Server Protected
protected authResult =
  case authResult of
    (Server.Authenticated user) ->
      return (name user) :<|>
      return (email user)
    x ->
      throwAll err401{ errBody = BS.pack $ show  x }


public :: CookieSettings -> JWTSettings -> Server Public
public cs jwts =
  checkCredentials cs jwts :<|>
  Servant.serveDirectoryFileServer "./"


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


checkCredentials :: CookieSettings -> JWTSettings -> Login -> Handler CredHeaders
checkCredentials cookieSettings jwtSettings (Login "admin" "admin") = do
   let usr =
         User "Ali Baba" "ali@email.com"
   maybeAddCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
   case maybeAddCookies of
     Nothing ->
       throwError err401
     Just addCookies ->
       return $ addCookies usr
checkCredentials _ _ _ =
  throwError err401
