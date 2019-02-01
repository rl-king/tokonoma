module Data.Req exposing
    ( delete
    , deleteTask
    , get
    , getTask
    , postTask
    , postTaskNoContent
    )

import File exposing (File)
import Http
import Json.Decode as Decode
import Task exposing (Task)
import Url.Builder exposing (absolute)


get : String -> Decode.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
get url decoder msg =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson msg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


delete : String -> (Result Http.Error () -> msg) -> Cmd msg
delete url msg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }


getTask : String -> Decode.Decoder a -> Task Http.Error a
getTask url decoder =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = expectJson decoder
        , timeout = Nothing
        }


postTask : String -> Http.Body -> Decode.Decoder a -> Task Http.Error a
postTask url body decoder =
    Http.task
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , resolver = expectJson decoder
        , timeout = Nothing
        }


deleteTask : String -> Http.Body -> Task Http.Error ()
deleteTask url body =
    Http.task
        { method = "DELETE"
        , headers = []
        , url = url
        , body = body
        , resolver = expectNoContent
        , timeout = Nothing
        }


postTaskNoContent : String -> Http.Body -> Task Http.Error ()
postTaskNoContent url body =
    Http.task
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , resolver = expectNoContent
        , timeout = Nothing
        }


expectNoContent : Http.Resolver Http.Error ()
expectNoContent =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ _ ->
                    Ok ()


expectJson : Decode.Decoder a -> Http.Resolver Http.Error a
expectJson decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Decode.errorToString err))
