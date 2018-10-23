module Main exposing (main)

import Browser
import Browser.Navigation
import Css exposing (..)
import Css.Breakpoint as Breakpoint
import Css.Global as Global exposing (global)
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( attribute
        , autofocus
        , css
        , disabled
        , href
        , placeholder
        , style
        , target
        , type_
        , value
        )
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Html.Styled.Keyed as Keyed
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Markdown
import String.Interpolate exposing (interpolate)
import Task exposing (Task)
import Url exposing (Url)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }



-- MODEL


type alias Model =
    { key : Browser.Navigation.Key
    , auth : Auth
    , username : String
    , password : String
    , title : String
    , resources : List Resource
    }


type Auth
    = Auth User
    | Anonymous
    | Loading


init : flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ location key =
    ( { key = key
      , auth = Loading
      , username = ""
      , password = ""
      , title = ""
      , resources = []
      }
    , Http.send GotUser <|
        Http.get "/status" decodeUser
    )



-- UPDATE


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | OnUsernameInput String
    | OnPasswordInput String
    | OnTitleInput String
    | PerformLogin
    | PerformPost
    | GotUser (Result Http.Error User)
    | Logout
    | GotLogout (Result Http.Error ())
    | GotNewPost (Result Http.Error ())
    | GotResources (Result Http.Error (List Resource))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlChange location ->
            ( model, Cmd.none )

        OnUrlRequest (Browser.Internal url) ->
            ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

        OnUrlRequest (Browser.External href) ->
            ( model, Browser.Navigation.load href )

        OnUsernameInput input ->
            ( { model | username = input }, Cmd.none )

        OnPasswordInput input ->
            ( { model | password = input }, Cmd.none )

        OnTitleInput input ->
            ( { model | title = input }, Cmd.none )

        PerformLogin ->
            let
                body =
                    Encode.object
                        [ ( "username", Encode.string model.username )
                        , ( "password", Encode.string model.password )
                        ]
            in
            ( model
            , Http.send GotUser <|
                Http.post "/login" (Http.jsonBody body) decodeUser
            )

        PerformPost ->
            ( model
            , Task.attempt GotResources <|
                Task.andThen (\_ -> getResources) <|
                    postNewResource (Encode.string model.title)
            )

        GotUser (Ok user) ->
            ( { model | auth = Auth user }
            , Task.attempt GotResources getResources
            )

        GotUser res ->
            ( { model | auth = Anonymous }, Cmd.none )

        Logout ->
            ( model
            , Http.send GotLogout logout
            )

        GotLogout (Ok _) ->
            ( { model | auth = Anonymous }, Cmd.none )

        GotLogout (Err _) ->
            ( model, Cmd.none )

        GotNewPost _ ->
            ( model, Cmd.none )

        GotResources (Ok resources) ->
            ( { model | resources = resources }, Cmd.none )

        GotResources (Err _) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Tokonoma"
    , body =
        [ toUnstyled <|
            main_ [ css styling.main ] ([ global globalStyling ] ++ viewBody model)
        ]
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    case model.auth of
        Loading ->
            []

        Anonymous ->
            viewLogin model

        Auth user ->
            viewAdmin user model


viewAdmin : User -> Model -> List (Html Msg)
viewAdmin { username } model =
    [ header [ css styling.header ]
        [ h1 [] [ text "Tokonoma" ]
        , section [ css styling.headerUser ]
            [ h4 [] [ text username ]
            , button [ onClick Logout, css styling.logout ] [ text "Logout" ]
            ]
        ]
    , section [ css styling.content ]
        [ viewNewResource
        , viewResources model.resources
        ]
    ]


viewResources : List Resource -> Html Msg
viewResources resources =
    ul [] <|
        List.map viewResource resources


viewResource : Resource -> Html Msg
viewResource { title, id } =
    li [ css styling.resource ]
        [ h2 [] [ text title ]
        , span [] [ text (String.fromInt id) ]
        ]


viewLogin : Model -> List (Html Msg)
viewLogin model =
    let
        disable =
            String.isEmpty model.password
                || String.isEmpty model.username
    in
    [ section [ css styling.login ]
        [ Html.Styled.form [ onSubmit PerformLogin ]
            [ label [] [ text "Username" ]
            , input [ onInput OnUsernameInput, autofocus True ] []
            , label [] [ text "Password" ]
            , input [ onInput OnPasswordInput, type_ "password" ] []
            , button [ disabled disable ] [ text "Login" ]
            ]
        ]
    ]


viewNewResource : Html Msg
viewNewResource =
    Html.Styled.form [ onSubmit PerformPost, css styling.newResource ]
        [ label [] [ text "New Resource" ]
        , input [ onInput OnTitleInput, placeholder "Title" ] []
        , button [] [ text "Save" ]
        ]



-- DECODE


type alias User =
    { username : String
    , email : String
    }


decodeUser : Decode.Decoder User
decodeUser =
    Decode.map2 User
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)


type alias Resource =
    { id : Int
    , title : String
    }


decodeResource : Decode.Decoder Resource
decodeResource =
    Decode.map2 Resource
        (Decode.field "_id" Decode.int)
        (Decode.field "_title" Decode.string)



-- HTTP


getResources : Task Http.Error (List Resource)
getResources =
    Http.toTask <|
        Http.get "/resources" (Decode.list decodeResource)


postNewResource : Encode.Value -> Task Http.Error ()
postNewResource body =
    Http.toTask <|
        Http.request
            { method = "POST"
            , headers = []
            , url = "/resources"
            , body = Http.jsonBody body
            , expect = Http.expectStringResponse (\_ -> Ok ())
            , timeout = Nothing
            , withCredentials = False
            }


logout : Http.Request ()
logout =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/logout"
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }



-- LINK
-- STYLING


colors =
    { lightGrey = hex "f0f0f0"
    , grey = hex "2f2f2f"
    , greyLighter = hex "3f3f3f"
    , darkGrey = hex "1f1f1f"
    , black = hex "111"
    , white = hex "fff"
    , green = hex "43DCC1"
    , red = hex "FD3740"
    , yellow = hex "F1D027"
    , blue = hex "3CA5EA"
    , offwhite = hex "fafafa"
    , purple = hex "7F63D2"
    , pink = hex "f9b2e1"
    }


styling =
    { main =
        [ Breakpoint.small []
        ]
    , header =
        [ color colors.white
        , backgroundColor colors.black
        , padding2 (rem 1) (rem 2)
        , displayFlex
        , justifyContent spaceBetween
        , alignItems center
        ]
    , headerUser =
        [ displayFlex
        , justifyContent flexEnd
        , alignItems center
        , Global.descendants
            [ Global.button
                [ marginLeft (rem 1) ]
            ]
        ]
    , content =
        [ padding (rem 2)
        ]
    , resource =
        [ padding (rem 1)
        , backgroundColor colors.white
        , marginTop (rem 0.5)
        ]
    , newResource =
        [ padding (rem 1)
        , backgroundColor colors.white
        , marginTop (rem 0.5)
        , boxShadow4 (rem 0.5) (rem 0.5) zero colors.lightGrey
        , marginBottom (rem 2)
        ]
    , login =
        [ backgroundColor colors.white
        , padding (rem 1)
        , width (rem 25)
        , margin2 (vh 25) auto
        , Global.descendants
            [ Global.button
                [ backgroundColor colors.grey
                , padding2 (rem 0.5) (rem 1)
                , color colors.white
                , width (pct 100)
                , Css.disabled
                    [ color colors.greyLighter
                    ]
                ]
            ]
        ]
    , logout =
        [ backgroundColor colors.darkGrey
        , color colors.red
        ]
    }


globalStyling =
    [ Global.everything
        [ boxSizing borderBox ]
    , Global.html
        [ margin zero
        , padding zero
        , backgroundColor colors.offwhite
        ]
    , Global.body
        [ margin zero
        , padding zero
        , fontSize (pct 87.5)
        , fontFamilies
            [ "-apple-system"
            , "BlinkMacSystemFont"
            , "Segoe UI"
            , "Roboto"
            , "Oxygen"
            , "Ubuntu"
            , "Cantarell"
            , "Fira Sans"
            , "Droid Sans"
            , "Helvetica Neue"
            , "sans-serif"
            ]
        ]
    , Global.h1
        [ margin2 (rem 0.25) zero
        , fontSize (rem 1.25)
        , fontWeight (int 500)
        ]
    , Global.h2
        [ margin2 (rem 0.25) zero
        , fontWeight (int 500)
        ]
    , Global.h4
        [ margin2 (rem 0.25) zero
        , fontWeight (int 500)
        ]
    , Global.ul
        [ listStyle none
        , padding zero
        , margin zero
        ]
    , Global.code
        [ fontSize (rem 0.875)
        , lineHeight (rem 1.5)
        , padding zero
        , fontFamilies
            [ "Iosevka SS08 Web"
            , "monospace"
            ]
        ]
    , Global.p [ margin3 (rem 0.15) zero (rem 0.25) ]
    , Global.pre [ display none ]
    , Global.a
        [ textDecoration none
        , color colors.black
        , hover [ textDecoration underline ]
        ]
    , Global.img
        [ maxWidth (rem 5)
        , width (pct 100)
        , margin2 (rem 1) zero
        , display none
        ]
    , Global.button
        [ backgroundColor colors.lightGrey
        , padding2 (rem 0.5) (rem 1.5)
        , border zero
        , fontWeight (int 500)
        , fontSize (rem 1)
        , cursor pointer
        ]
    , Global.label
        [ display block
        , marginBottom (rem 0.5)
        , fontSize (rem 1)
        , fontWeight (int 500)
        ]
    , Global.input
        [ display block
        , border zero
        , height (rem 2.5)
        , width (pct 100)
        , marginBottom (rem 1.5)
        , fontSize (rem 1)
        , padding2 (rem 0) (rem 0.5)
        , backgroundColor colors.lightGrey
        ]
    ]
