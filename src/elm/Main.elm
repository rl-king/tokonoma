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
            let
                body =
                    Encode.object
                        [ ( "_id", Encode.int 1 )
                        , ( "_title", Encode.string model.title )
                        ]
            in
            ( model
            , Http.send GotNewPost <|
                Http.post "/resources" (Http.jsonBody body) (Decode.succeed ())
            )

        GotUser (Ok user) ->
            ( { model | auth = Auth user }, Cmd.none )

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



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Tokonoma"
    , body =
        [ toUnstyled <|
            main_ [ css styling.main ] [ global globalStyling, viewBody model ]
        ]
    }


viewBody : Model -> Html Msg
viewBody model =
    case model.auth of
        Loading ->
            text ""

        Anonymous ->
            viewLogin

        Auth user ->
            viewAdmin model


viewAdmin : Model -> Html Msg
viewAdmin model =
    section []
        [ text (Debug.toString model.auth)
        , h1 [] [ text "Tokonoma" ]
        , button [ onClick Logout ] [ text "Logout" ]
        , viewPost
        ]


viewLogin : Html Msg
viewLogin =
    section [ css styling.login ]
        [ Html.Styled.form [ onSubmit PerformLogin ]
            [ label [] [ text "Username" ]
            , input [ onInput OnUsernameInput, autofocus True ] []
            , label [] [ text "Password" ]
            , input [ onInput OnPasswordInput ] []
            , button [] [ text "Login" ]
            ]
        ]


viewPost : Html Msg
viewPost =
    Html.Styled.form [ onSubmit PerformPost ]
        [ label [] [ text "New Resource" ]
        , input [ onInput OnTitleInput ] []
        , button [] [ text "Post" ]
        ]



-- LINK
-- STYLING


colors =
    { lightGrey = hex "f0f0f0"
    , grey = hex "2f2f2f"
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
    , login =
        [ backgroundColor colors.white
        , boxShadow4 (rem 0.5) (rem 0.5) zero colors.lightGrey
        , padding (rem 1)
        , width (rem 25)
        , margin2 (vh 25) auto
        , Global.descendants
            [ Global.button
                [ backgroundColor colors.green
                , padding2 (rem 0.5) (rem 1)
                , color colors.white
                , width (pct 100)
                ]
            ]
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
    , Global.h1 [ margin2 (rem 0.25) zero ]
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



-- HTTP


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
