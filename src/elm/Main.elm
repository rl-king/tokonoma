port module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Css exposing (..)
import Css.Breakpoint as Breakpoint
import Css.Global as Global exposing (global)
import Data.Auth as Auth exposing (Auth)
import Data.Request as Request
import Data.Resource as Resource exposing (Resource)
import Data.Session as Session
import Data.User as User exposing (User)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Page.Edit as Edit
import Page.Login as Login
import Page.Resources as Resources
import Specification exposing (colors)
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
import View.Header



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
    Sub.map EditMsg Edit.subscriptions



-- MODEL


type alias Model =
    { page : Page
    }


type Page
    = Resources Resources.Model
    | Edit Edit.Model
    | Login Login.Model
    | NotFound Session.Data
    | Status Session.Data
    | Redirect Session.Data


init : flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { page = Status (Session.init key)
      }
    , Task.attempt (GotStatus url) Request.getStatus
    )



-- UPDATE


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | LoginMsg Login.Msg
    | ResourcesMsg Resources.Msg
    | EditMsg Edit.Msg
    | PerformLogout
    | GotStatus Url (Result Http.Error User)
    | GotLogout (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlChange url ->
            onNavigation url model

        OnUrlRequest (Browser.External href) ->
            ( model, Navigation.load href )

        OnUrlRequest (Browser.Internal url) ->
            let
                key =
                    Session.getNavKey (toSession model)
            in
            ( model, Navigation.pushUrl key (Url.toString url) )

        LoginMsg loginMsg ->
            case model.page of
                Login loginModel ->
                    step model Login LoginMsg (Login.update loginMsg loginModel)

                _ ->
                    ( model, Cmd.none )

        ResourcesMsg resourcesMsg ->
            case model.page of
                Resources resourcesModel ->
                    step model Resources ResourcesMsg <|
                        Resources.update resourcesMsg resourcesModel

                _ ->
                    ( model, Cmd.none )

        EditMsg editMsg ->
            case model.page of
                Edit editModel ->
                    step model Edit EditMsg <|
                        Edit.update editMsg editModel

                _ ->
                    ( model, Cmd.none )

        PerformLogout ->
            ( model
            , Task.attempt GotLogout Request.postLogout
            )

        GotLogout result ->
            let
                session =
                    toSession model
            in
            ( { model | page = Redirect (Session.new session) }
            , Navigation.pushUrl (Session.getNavKey session) "/login"
            )

        GotStatus url result ->
            let
                key =
                    Session.getNavKey (toSession model)
            in
            case Auth.fromResult result of
                Auth.Anonymous ->
                    let
                        queryParams =
                            if url.path == "/login" then
                                [ Builder.string "redirect" "/" ]

                            else
                                [ Builder.string "redirect" (Url.toString url) ]
                    in
                    ( model
                    , Navigation.replaceUrl key <|
                        Builder.absolute [ "login" ] queryParams
                    )

                Auth.Auth user ->
                    let
                        session =
                            toSession model
                    in
                    onNavigation url
                        { model | page = Redirect (Session.insertAuth (Auth.Auth user) session) }

                Auth.Unknown ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Tokonoma"
    , body =
        [ toUnstyled <|
            div []
                [ global globalStyling
                , viewPage model
                ]
        ]
    }


viewPage : Model -> Html Msg
viewPage model =
    let
        wrap msg pageView =
            section []
                [ View.Header.view PerformLogout (toSession model)
                , Html.Styled.map msg pageView
                ]
    in
    case model.page of
        Login loginModel ->
            Html.Styled.map LoginMsg <|
                Login.view loginModel

        Resources resourcesModel ->
            wrap ResourcesMsg <|
                Resources.view resourcesModel

        Edit editModel ->
            wrap EditMsg <|
                Edit.view editModel

        NotFound _ ->
            text "NotFound"

        Status _ ->
            text ""

        Redirect _ ->
            text ""



-- ROUTING


toSession : Model -> Session.Data
toSession model =
    case model.page of
        Resources m ->
            m.session

        Edit m ->
            m.session

        Login m ->
            m.session

        Status s ->
            s

        Redirect s ->
            s

        NotFound s ->
            s


onNavigation : Url.Url -> Model -> ( Model, Cmd Msg )
onNavigation url model =
    let
        session =
            toSession model

        parser =
            Parser.oneOf
                [ route Parser.top <|
                    step model Resources ResourcesMsg (Resources.init session)
                , route (Parser.s "edit") <|
                    step model Edit EditMsg (Edit.init session)
                , route (Parser.s "login" <?> Query.string "redirect") <|
                    (step model Login LoginMsg << Login.init session)
                ]

        loginParser =
            route (Parser.s "login" <?> Query.string "redirect") <|
                (step model Login LoginMsg << Login.init session)
    in
    case Session.getAuth session of
        Auth.Auth user ->
            Parser.parse parser url
                |> Maybe.withDefault
                    ( { model | page = NotFound session }
                    , Cmd.none
                    )

        _ ->
            Parser.parse loginParser url
                |> Maybe.withDefault
                    (step model Login LoginMsg (Login.init session Nothing))


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


step : Model -> (a -> Page) -> (msg -> Msg) -> ( a, Cmd msg ) -> ( Model, Cmd Msg )
step model toPage toMsg ( page, cmds ) =
    ( { model | page = toPage page }
    , Cmd.map toMsg cmds
    )



-- STYLING


globalStyling =
    [ Global.everything
        [ boxSizing borderBox ]
    , Global.html
        [ margin zero
        , padding zero
        , backgroundColor colors.white
        , fontSize (pct 87.5)
        , lineHeight (rem 1.5)
        ]
    , Global.body
        [ margin zero
        , padding zero
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
