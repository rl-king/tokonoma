module Data.User exposing
    ( User
    , decode
    )

import Json.Decode as Decode


type alias User =
    { username : String
    , email : String
    }


decode : Decode.Decoder User
decode =
    Decode.map2 User
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)
