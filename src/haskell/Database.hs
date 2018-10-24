{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Database where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)


-- DATABASE


type Database =
  Map Int Resource


init :: Database
init =
  Map.fromList
  [ (1, Resource 1 "Hello world" True 1540357150428)
  , (2, Resource 2 "Foo bar" True 1540357150428)
  ]


insert :: Text -> Int -> Database -> Database
insert title now db =
  Map.insert uid (Resource uid title True now) db
  where uid = (+1) . maximum $ Map.keys db


delete :: Int -> Database -> Database
delete =
  Map.delete


all :: Database -> [Resource]
all =
  Map.elems


-- RESOURCE


data Resource =
  Resource
  { _id :: Int
  , _title :: Text
  , _published :: Bool
  , _created :: Int -- Posix time
  } deriving (Eq, Show, Read, Generic)


instance ToJSON Resource
instance FromJSON Resource
