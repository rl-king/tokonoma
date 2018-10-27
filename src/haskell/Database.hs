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
  [ (1, Resource 1 "Hello world" ipsum1 True 1540357150428)
  , (2, Resource 2 "Foo bar" ipsum2 True 1540357150428)
  ]


insert :: NewResource -> Int -> Database -> Database
insert (NewResource title body) now db =
  Map.insert uid (Resource uid title body True now) db
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
  , _body :: Text
  , _published :: Bool
  , _created :: Int -- Posix time
  } deriving (Eq, Show, Read, Generic)


instance ToJSON Resource
instance FromJSON Resource


data NewResource =
  NewResource
  { _ntitle :: Text
  , _nbody :: Text
  } deriving (Eq, Show, Read, Generic)


instance ToJSON NewResource
instance FromJSON NewResource


data NewFile=
  NewFile
  { _nfilename :: Text
  , _ncontent :: Text
  } deriving (Eq, Show, Read, Generic)


instance ToJSON NewFile
instance FromJSON NewFile


-- IPSUM


ipsum1 :: Text
ipsum1 =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed malesuada lobortis arcu, sollicitudin lacinia erat. Nam at lobortis ex. Sed consequat, purus ut pellentesque cursus, nisi enim pretium ligula, quis cursus sem tellus sed lorem. Lorem ipsum dolor sit amet, consectetur adipiscing elit."


ipsum2 :: Text
ipsum2  =
  "Aenean turpis felis, feugiat eget augue sed, venenatis dapibus magna. Aliquam pharetra commodo neque, ac maximus nunc ultricies at. Donec lobortis porttitor tortor, et porttitor nunc pulvinar at. In eleifend sodales varius. Integer vel lorem mi. Curabitur vitae facilisis nisl. In hac habitasse platea dictumst."
