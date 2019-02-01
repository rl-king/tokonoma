module Data.Resource exposing
    ( Resource
    , decode
    )

import Data.FileData exposing (FileData)
import File exposing (File)
import Json.Decode as Decode
import Time


type alias Resource =
    { id : Int
    , title : String
    , body : String
    , published : Bool
    , created : Time.Posix
    , files : List FileData
    }


decode : Decode.Decoder Resource
decode =
    Decode.map6 Resource
        (Decode.field "_id" Decode.int)
        (Decode.field "_title" Decode.string)
        (Decode.field "_body" Decode.string)
        (Decode.field "_published" Decode.bool)
        (Decode.field "_created" decodePosix)
        (Decode.field "_files" (Decode.list Data.FileData.decode))


decodePosix : Decode.Decoder Time.Posix
decodePosix =
    Decode.int
        |> Decode.map Time.millisToPosix
