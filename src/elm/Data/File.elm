module Data.File exposing
    ( File
    , decode
    , encode
    )

import Json.Decode as Decode
import Json.Encode as Encode


type alias File =
    { filename : String
    , path : String
    }


encode : File -> Encode.Value
encode file =
    Encode.object
        [ ( "_filename", Encode.string file.filename )
        , ( "_path", Encode.string file.path )
        ]


decode : Decode.Decoder File
decode =
    Decode.map2 File
        (Decode.field "_filename" Decode.string)
        (Decode.field "_path" Decode.string)
