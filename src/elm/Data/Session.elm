module Data.Session exposing
    ( Data
    , deleteResource
    , getAuth
    , getNavKey
    , getResources
    , init
    , insertAuth
    , insertResources
    )

import Browser.Navigation as Navigation
import Data.Auth exposing (Auth(..))
import Data.Resource exposing (Resource)
import Dict exposing (Dict)



-- DATA


type Data
    = Data
        { auth : Auth
        , navKey : Navigation.Key
        , resources : Dict Int Resource
        }



-- INSERT


init : Navigation.Key -> Data
init key =
    Data
        { auth = Anonymous
        , navKey = key
        , resources = Dict.empty
        }


insertAuth : Auth -> Data -> Data
insertAuth a (Data data) =
    Data { data | auth = a }


insertResources resources (Data data) =
    List.foldl (\rsc acc -> Dict.insert rsc.id rsc acc) data.resources resources
        |> (\newResources -> Data { data | resources = newResources })



-- GET


getNavKey : Data -> Navigation.Key
getNavKey (Data data) =
    data.navKey


getAuth : Data -> Auth
getAuth (Data data) =
    data.auth


getResources : Data -> List Resource
getResources (Data data) =
    List.sortBy (negate << .id) <|
        List.map Tuple.second <|
            Dict.toList data.resources



-- DELETE


deleteResource : Int -> Data -> Data
deleteResource id (Data data) =
    Data { data | resources = Dict.remove id data.resources }
