{-# LANGUAGE DeriveGeneric #-}
module Server.Log where

import Data.Aeson (FromJSON, ToJSON, toEncoding, defaultOptions, encode, genericToEncoding)
import Data.Text (Text)
import qualified Data.Time.Clock as Time
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Servant.Auth.Server as Server
import qualified System.Log.FastLogger as Log


data LogMessage =
  LogMessage
  { message :: !Text
  , timestamp :: !Time.UTCTime
  } deriving (Eq, Show, Generic)


instance FromJSON LogMessage
instance ToJSON LogMessage
  where toEncoding = genericToEncoding defaultOptions


instance Log.ToLogStr LogMessage
  where toLogStr = Log.toLogStr . encode


jsonRequestLogger :: IO Middleware
jsonRequestLogger =
  mkRequestLogger $
  def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }
