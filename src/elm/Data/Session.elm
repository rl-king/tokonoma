module Data.Session exposing
    ( Data
    , auth
    , init
    , insertAuth
    , navKey
    )

import Browser.Navigation as Navigation
import Data.Auth exposing (Auth(..))



-- DATA


type Data
    = Data
        { navKey : Navigation.Key
        , auth : Auth
        }



-- INSERT


init : Navigation.Key -> Data
init key =
    Data
        { navKey = key
        , auth = Anonymous
        }


insertAuth : Auth -> Data -> Data
insertAuth a (Data data) =
    Data { data | auth = a }



-- GET


auth : Data -> Auth
auth (Data data) =
    data.auth


navKey : Data -> Navigation.Key
navKey (Data data) =
    data.navKey
