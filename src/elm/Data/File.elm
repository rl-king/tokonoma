module Data.File exposing (File, decodeFileUpload)

import Json.Decode as Decode
import Json.Encode as Encode


type alias File =
    { path : String
    , filename : String
    }


decodeFileUpload : Decode.Value -> Result Decode.Error (List File)
decodeFileUpload =
    Decode.decodeValue
        (Decode.map (List.map (\y -> File y y)) <|
            Decode.list Decode.string
        )
