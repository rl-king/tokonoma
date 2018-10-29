module Page.Resources exposing (Model, Msg, init, update, view)

import Css exposing (..)
import Css.Breakpoint as Breakpoint
import Css.Global as Global exposing (global)
import Data.Request as Request
import Data.Resource as Resource exposing (Resource)
import Data.Session as Session
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
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
    ( { session = session
      }
    , Task.attempt GotResources Request.getResources
    )



-- UPDATE


type Msg
    = DeleteResource Int
    | GotResources (Result Http.Error (List Resource))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeleteResource id ->
            ( { model | session = Session.deleteResource id model.session }
            , Task.attempt GotResources <|
                Task.andThen (\_ -> Request.getResources) <|
                    Request.deleteResource id
            )

        GotResources (Ok resources) ->
            ( { model | session = Session.insertResources resources model.session }, Cmd.none )

        GotResources (Err err) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    section [] <|
        List.map viewResource <|
            Session.getResources model.session


viewResource : Resource -> Html Msg
viewResource { title, id, created, body } =
    div [ css styling.resource ]
        [ section []
            [ span [] [ text "Id: ", text (String.fromInt id) ]
            , text " | "
            , time []
                [ text "Created: "
                , text (String.fromInt (Time.posixToMillis created))
                , h2 [] [ text title ]
                ]
            , Html.Styled.fromUnstyled <|
                Markdown.toHtml [] body
            ]
        , button [ onClick (DeleteResource id) ] [ text "delete" ]
        ]


styling =
    { resource =
        [ padding (rem 1)
        , backgroundColor colors.white
        , marginTop (rem 0.5)
        , displayFlex
        , justifyContent spaceBetween
        , alignItems flexEnd
        , Global.descendants
            [ Global.button
                [ color colors.red
                , backgroundColor transparent
                ]
            ]
        ]
    }
