port module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Css exposing (..)
import Css.Breakpoint as Breakpoint
import Css.Global as Global exposing (global)
import Data.Auth as Auth exposing (Auth)
import Data.File as File exposing (File)
import Data.Request as Request
import Data.Resource as Resource exposing (Resource)
import Data.Session as Session
import Data.Update as Update
import Data.User as User exposing (User)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (on, onClick, onInput, onSubmit)
import Html.Styled.Keyed as Keyed
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Page.Edit
import Page.Login
import Page.Resources
import Set exposing (Set)
import Specification exposing (colors)
import String.Interpolate exposing (interpolate)
import Task exposing (Task)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser
    exposing
        ( (</>)
        , (<?>)
        , Parser
        )
import Url.Parser.Query as Query



-- PORTS


port onFileUpload : (Decode.Value -> msg) -> Sub msg



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- onFileUpload
--     (Response
--         << FileUpload
--         << File.decodeFileUpload
--     )
-- MODEL


type alias Model =
    { session : Session.Data
    , page : Page
    }


type Page
    = Resources Page.Resources.Model
    | Edit Page.Edit.Model
    | Login Page.Login.Model
    | Loading
    | NotFound


init : flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    onNavigation url { session = Session.init key, page = Loading }
        |> (\( model, cmds ) ->
                ( model, Cmd.batch [ Task.attempt GotStatus Request.getStatus, cmds ] )
           )



-- UPDATE


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | LoginMsg Page.Login.Msg
    | ResourcesMsg Page.Resources.Msg
    | PerformLogout
    | GotStatus (Result Http.Error User)
    | GotLogout (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ session } as model) =
    case msg of
        OnUrlChange location ->
            ( model, Cmd.none )

        OnUrlRequest (Browser.Internal url) ->
            ( model, Navigation.pushUrl (Session.navKey session) (Url.toString url) )

        OnUrlRequest (Browser.External href) ->
            ( model, Navigation.load href )

        LoginMsg loginMsg ->
            case model.page of
                Login loginModel ->
                    step model Login LoginMsg (Page.Login.update loginMsg loginModel)

                _ ->
                    ( model, Cmd.none )

        ResourcesMsg resourcesMsg ->
            case model.page of
                Resources resourcesModel ->
                    step model Resources ResourcesMsg <|
                        Page.Resources.update resourcesMsg resourcesModel

                _ ->
                    ( model, Cmd.none )

        PerformLogout ->
            ( model
            , Task.attempt GotLogout Request.postLogout
            )

        GotLogout result ->
            ( model
            , Cmd.none
            )

        GotStatus result ->
            ( { model
                | session =
                    Session.insertAuth (Auth.fromResult result) model.session
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Tokonoma"
    , body =
        [ toUnstyled <|
            main_ [ css styling.main ]
                [ global globalStyling
                , viewPage model
                ]
        ]
    }


viewPage : Model -> Html Msg
viewPage model =
    let
        page =
            case model.page of
                Login loginModel ->
                    Html.Styled.map LoginMsg <|
                        Page.Login.view loginModel

                _ ->
                    text "tododo"
    in
    div []
        [ header [ css styling.header ]
            [ h1 [] [ text "Tokonoma" ]
            , section [ css styling.headerUser ]
                -- [ h4 [] [ text username ]
                [ button [ onClick PerformLogout, css styling.logout ] [ text "Logout" ]
                ]
            ]
        , section [ css styling.content ] [ page ]
        ]



-- ROUTING


onNavigation : Url.Url -> Model -> ( Model, Cmd Msg )
onNavigation url model =
    let
        parser =
            Parser.oneOf
                [ route Parser.top
                    (step model Resources ResourcesMsg (Page.Resources.init model.session))

                -- , Parser.map Edit (Parser.s "edit")
                ]
    in
    case Session.auth model.session of
        Auth.Auth user ->
            Parser.parse parser url
                |> Maybe.withDefault
                    ( { model | page = NotFound }
                    , Cmd.none
                    )

        _ ->
            step model Login LoginMsg (Page.Login.init model.session)


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


step : Model -> (page -> Page) -> (msg -> Msg) -> ( page, Cmd msg ) -> ( Model, Cmd Msg )
step model toPage toMsg ( page, cmds ) =
    ( { model | page = toPage page }
    , Cmd.map toMsg cmds
    )



-- STYLING


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
        , lineHeight (rem 1.5)
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
        [ fontSize (rem 1)
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
    , Global.p
        [ margin3 (rem 0.15) zero (rem 0.25)
        , fontSize (rem 1)
        ]
    , Global.pre [ display none ]
    , Global.a
        [ textDecoration none
        , color colors.black
        , hover [ textDecoration underline ]
        ]
    , Global.img
        [ maxWidth (rem 10)
        , width (pct 100)
        , margin2 (rem 1) zero
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
    , Global.textarea
        [ display block
        , border zero
        , height (rem 10)
        , width (pct 100)
        , marginBottom (rem 1.5)
        , fontSize (rem 1)
        , padding (rem 0.5)
        , lineHeight (rem 1.5)
        , backgroundColor colors.lightGrey
        ]
    ]
