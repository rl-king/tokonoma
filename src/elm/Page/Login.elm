module Page.Login exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Css exposing (..)
import Css.Breakpoint as Breakpoint
import Css.Global as Global exposing (global)
import Data.Auth as Auth exposing (Auth(..))
import Data.Request as Request
import Data.Resource as Resource exposing (Resource)
import Data.Session as Session
import Data.User exposing (User)
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( autofocus
        , css
        , disabled
        , placeholder
        , type_
        )
import Html.Styled.Events exposing (on, onClick, onInput, onSubmit)
import Html.Styled.Keyed as Keyed
import Http
import Markdown
import Specification exposing (colors)
import Task
import Time



-- MODEL


type alias Model =
    { session : Session.Data
    , redirectUrl : Maybe String
    , username : String
    , password : String
    }


init : Session.Data -> Maybe String -> ( Model, Cmd msg )
init session redirectUrl =
    let
        redirect =
            case Session.getAuth session of
                Auth.Auth _ ->
                    Navigation.replaceUrl (Session.getNavKey session) "/"

                _ ->
                    Cmd.none
    in
    ( Model session redirectUrl "" ""
    , redirect
    )



-- UPDATE


type Msg
    = OnUsernameInput String
    | OnPasswordInput String
    | PerformLogin
    | GotPerformLogin (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUsernameInput input ->
            ( { model | username = input }, Cmd.none )

        OnPasswordInput input ->
            ( { model | password = input }, Cmd.none )

        PerformLogin ->
            ( model
            , Task.attempt GotPerformLogin <|
                Request.postLogin model.username model.password
            )

        GotPerformLogin (Ok user) ->
            let
                url =
                    Maybe.withDefault "/" model.redirectUrl
            in
            ( { model | session = Session.insertAuth (Auth user) model.session }
            , Navigation.replaceUrl (Session.getNavKey model.session) url
            )

        GotPerformLogin (Err err) ->
            ( { model | session = Session.insertAuth Anonymous model.session }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        disable =
            String.isEmpty model.password
                || String.isEmpty model.username
    in
    section [ css styling.login ]
        [ Html.Styled.form [ onSubmit PerformLogin ]
            [ input
                [ onInput OnUsernameInput
                , autofocus True
                , placeholder "Username"
                ]
                []
            , input
                [ onInput OnPasswordInput
                , type_ "password"
                , placeholder "Password"
                ]
                []
            , button [ disabled disable ] [ text "Login" ]
            ]
        ]


styling =
    { login =
        [ backgroundColor colors.white
        , padding (rem 1)
        , width (rem 25)
        , margin2 (vh 25) auto
        , Global.descendants
            [ Global.button
                [ backgroundColor colors.blue
                , padding2 (rem 0.5) (rem 1)
                , color colors.white
                , width (pct 100)
                , Css.disabled
                    [ color colors.greyLighter
                    , cursor notAllowed
                    ]
                ]
            ]
        ]
    }
