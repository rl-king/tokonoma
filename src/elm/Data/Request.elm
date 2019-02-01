module Data.Request exposing
    ( deleteResource
    , getResource
    , getResources
    , getStatus
    , postFiles
    , postLogin
    , postLogout
    , postNewResource
    )

import Data.Req as Req
import Data.Resource as Resource exposing (Resource)
import Data.User as User exposing (User)
import File exposing (File)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
import Time



-- GET


getResources : Task Http.Error (List Resource)
getResources =
    Req.getTask "/resources" (Decode.list Resource.decode)


getResource : Int -> Task Http.Error Resource
getResource id =
    Req.getTask ("/resources/" ++ String.fromInt id) Resource.decode


getStatus : Task Http.Error User
getStatus =
    Req.getTask "/status" User.decode



-- POST


postLogin : String -> String -> Task Http.Error User
postLogin username password =
    let
        json =
            Encode.object
                [ ( "username", Encode.string username )
                , ( "password", Encode.string password )
                ]
    in
    Req.postTask "/login" (Http.jsonBody json) User.decode


postNewResource : String -> String -> List File -> Task Http.Error Int
postNewResource title body files =
    let
        json =
            Encode.object
                [ ( "_ntitle", Encode.string title )
                , ( "_nbody", Encode.string body )

                -- , ( "_nfiles", Encode.list File.encode files )
                ]
    in
    Req.postTask "/resources" (Http.jsonBody json) Decode.int


postFiles : List File -> Task Http.Error ()
postFiles files =
    Req.postTaskNoContent "/file" <|
        Http.multipartBody (List.map (Http.filePart "file") files)


postLogout : Task Http.Error ()
postLogout =
    Req.postTask "/logout" Http.emptyBody (Decode.succeed ())



-- DELETE


deleteResource : Int -> Task Http.Error ()
deleteResource id =
    Req.deleteTask ("/resources/" ++ String.fromInt id) Http.emptyBody
