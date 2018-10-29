module Page.Resources exposing (Model, Msg, init, update, view)

import Css exposing (..)
import Css.Breakpoint as Breakpoint
import Css.Global as Global exposing (global)
import Data.Request as Request
import Data.Resource as Resource exposing (Resource)
import Data.Session as Session
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)
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
    }


init : Session.Data -> ( Model, Cmd Msg )
init session =
    ( { session = session }
    , Task.attempt GotResources Request.getResources
    )



-- UPDATE


type Msg
    = GotResources (Result Http.Error (List Resource))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResources (Ok resources) ->
            ( { model | session = Session.insertResources resources model.session }, Cmd.none )

        GotResources (Err err) ->
            let
                _ =
                    Debug.log "rsc err" err
            in
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    section [ css styling.resources ] <|
        List.map viewResource <|
            Session.getResources model.session


viewResource : Resource -> Html Msg
viewResource { title, id, created, body, files } =
    div [ css styling.resource ]
        [ section []
            [ header []
                [ span [] [ text "Id: ", text (String.fromInt id) ]
                , text " | "
                , time []
                    [ text "Created: "
                    , text (String.fromInt (Time.posixToMillis created))
                    , h2 [] [ text title ]
                    ]
                ]
            , div [ css styling.content ]
                [ Html.Styled.fromUnstyled <|
                    Markdown.toHtml [] body
                , div [] (List.map (\{ path } -> img [ src path ] []) files)
                ]
            ]
        ]



-- STYLING


styling =
    { resources = [ padding (rem 2) ]
    , resource =
        [ backgroundColor colors.offwhite
        , marginBottom (rem 1)
        , Global.descendants
            [ Global.button
                [ color colors.white
                , backgroundColor colors.red
                ]
            , Global.header
                [ width (pct 100)
                , backgroundColor colors.lightGrey
                , padding2 (rem 0.5) (rem 1)
                ]
            , Global.footer
                [ width (pct 100)
                , backgroundColor colors.lightGrey
                , padding2 (rem 0.5) (rem 1)
                , displayFlex
                , justifyContent flexEnd
                ]
            , Global.img
                [ maxWidth (rem 10)
                , margin4 zero (rem 1) (rem 1) zero
                ]
            ]
        ]
    , content = [ padding2 (rem 1) (rem 1) ]
    }
