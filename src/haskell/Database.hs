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
  [ (1, Resource 1 "Hello")
  , (2, Resource 2 "World")
  ]


insert :: Text -> Database -> Database
insert title db =
  Map.insert uid (Resource uid title) db
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
  } deriving (Eq, Show, Read, Generic)


instance ToJSON Resource
instance FromJSON Resource
