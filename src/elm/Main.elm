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

        OnUrlRequest (Browser.Internal url) ->
            let
                key =
                    Session.getNavKey (toSession model)
            in
            ( model, Navigation.pushUrl key (Url.toString url) )

        OnUrlRequest (Browser.External href) ->
            ( model, Navigation.load href )

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
                key =
                    Session.getNavKey (toSession model)
            in
            ( model, Navigation.pushUrl key "/login" )

        GotStatus url result ->
            let
                key =
                    Session.getNavKey (toSession model)
            in
            case Auth.fromResult result of
                Auth.Anonymous ->
                    ( model
                    , Navigation.replaceUrl key <|
                        Builder.absolute [ "login" ] [ Builder.string "red" (Url.toString url) ]
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



-- ( {model = }, Navigation.pushUrl model.key "/login" )
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
                        Login.view loginModel

                Resources resourcesModel ->
                    Html.Styled.map ResourcesMsg <|
                        Resources.view resourcesModel

                Edit editModel ->
                    Html.Styled.map EditMsg <|
                        Edit.view editModel

                _ ->
                    text "tododo"
    in
    div []
        [ header [ css styling.header ]
            [ a [ href "/" ] [ h1 [] [ text "Tokonoma" ] ]
            , section [ css styling.headerUser ]
                -- [ h4 [] [ text username ]
                [ button [ onClick PerformLogout, css styling.logout ] [ text "Logout" ]
                ]
            ]
        , a [ href "/edit" ] [ text "Edit" ]
        , section [ css styling.content ] [ page ]
        ]



-- ROUTING
-- mapPageSession : (Session.Data -> Session.Data) -> Page -> Page
-- mapPageSession fn page =
--     case page of
--         Resources m ->
--             Resources { m | session = fn m.session }
--         Edit m ->
--             fn m.session
--         Login m ->
--             fn m.session
--         Loading s ->
--             fn
--                 s
--         NotFound s ->
--             fn s
--                 s


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
                , route (Parser.s "login") <|
                    step model Login LoginMsg (Login.init session)
                ]
    in
    case Session.getAuth session of
        Auth.Auth user ->
            Parser.parse parser url
                |> Maybe.withDefault
                    ( { model | page = NotFound session }
                    , Cmd.none
                    )

        _ ->
            step model Login LoginMsg (Login.init session)


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


step : Model -> (a -> Page) -> (msg -> Msg) -> ( a, Cmd msg ) -> ( Model, Cmd Msg )
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
        , Global.descendants
            [ Global.a [ color colors.white ] ]
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
