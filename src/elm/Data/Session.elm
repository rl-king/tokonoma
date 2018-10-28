module Data.Session exposing
    ( Data
    , auth
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
        , resources : Dict Int Resource
        }



-- INSERT


init : Data
init =
    Data
        { auth = Anonymous
        , resources = Dict.empty
        }


insertAuth : Auth -> Data -> Data
insertAuth a (Data data) =
    Data { data | auth = a }


insertResources resources (Data data) =
    List.foldl (\rsc acc -> Dict.insert rsc.id rsc acc) data.resources resources
        |> (\newResources -> Data { data | resources = newResources })



-- GET


auth : Data -> Auth
auth (Data data) =
    data.auth


getResources : Data -> List Resource
getResources (Data data) =
    List.sortBy (negate << .id) <|
        List.map Tuple.second <|
            Dict.toList data.resources
