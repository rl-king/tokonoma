module Data.FileData exposing
    ( FileData
    , decode
    , encode
    )

import Json.Decode as Decode
import Json.Encode as Encode


type alias FileData =
    { filename : String
    , path : String
    }


encode : FileData -> Encode.Value
encode file =
    Encode.object
        [ ( "_filename", Encode.string file.filename )
        , ( "_path", Encode.string file.path )
        ]


decode : Decode.Decoder FileData
decode =
    Decode.map2 FileData
        (Decode.field "_filename" Decode.string)
        (Decode.field "_path" Decode.string)
