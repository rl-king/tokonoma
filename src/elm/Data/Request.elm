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

import Data.File as File exposing (File)
import Data.Resource as Resource exposing (Resource)
import Data.User as User exposing (User)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
import Time



-- GET


getResources : Task Http.Error (List Resource)
getResources =
    Http.toTask <|
        Http.get "/resources" (Decode.list Resource.decode)


getResource : Int -> Task Http.Error Resource
getResource id =
    Http.toTask <|
        Http.get ("/resources/" ++ String.fromInt id)
            Resource.decode


getStatus : Task Http.Error User
getStatus =
    Http.toTask <|
        Http.get "/status" User.decode



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
    Http.toTask <|
        Http.post "/login" (Http.jsonBody json) User.decode


postNewResource : String -> String -> List File -> Task Http.Error Int
postNewResource title body files =
    let
        json =
            Encode.object
                [ ( "_ntitle", Encode.string title )
                , ( "_nbody", Encode.string body )
                , ( "_nfiles", Encode.list File.encode files )
                ]
    in
    Http.toTask <|
        Http.request
            { method = "POST"
            , headers = []
            , url = "/resources"
            , body = Http.jsonBody json
            , expect = Http.expectJson Decode.int
            , timeout = Nothing
            , withCredentials = False
            }


postFiles : List File -> Task Http.Error ()
postFiles files =
    let
        json { path, filename } =
            Encode.object
                [ ( "_npath", Encode.string path )
                , ( "_nfilename", Encode.string filename )
                ]
    in
    Http.toTask <|
        Http.request
            { method = "POST"
            , headers = []
            , url = "/file"
            , body = Http.jsonBody (Encode.list json files)
            , expect = Http.expectStringResponse (\_ -> Ok ())
            , timeout = Nothing
            , withCredentials = False
            }


postLogout : Task Http.Error ()
postLogout =
    Http.toTask <|
        Http.request
            { method = "POST"
            , headers = []
            , url = "/logout"
            , body = Http.emptyBody
            , expect = Http.expectStringResponse (\_ -> Ok ())
            , timeout = Nothing
            , withCredentials = False
            }



-- DELETE


deleteResource : Int -> Task Http.Error ()
deleteResource id =
    Http.toTask <|
        Http.request
            { method = "DELETE"
            , headers = []
            , url = "/resources/" ++ String.fromInt id
            , body = Http.emptyBody
            , expect = Http.expectStringResponse (\_ -> Ok ())
            , timeout = Nothing
            , withCredentials = False
            }
