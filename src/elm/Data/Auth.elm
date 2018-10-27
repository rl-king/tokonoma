module Data.Auth exposing (Auth(..), fromResult)

import Data.User as User exposing (User)
import Http


type Auth
    = Auth User
    | Anonymous
    | Loading


fromResult : Result Http.Error User -> Auth
fromResult result =
    case result of
        Ok user ->
            Auth user

        Err err ->
            Anonymous
