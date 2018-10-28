port module Page.Edit exposing (Model, Msg, init, update, view)

import Css exposing (..)
import Css.Breakpoint as Breakpoint
import Css.Global as Global exposing (global)
import Data.File exposing (File)
import Data.Request as Request
import Data.Resource as Resource exposing (Resource)
import Data.Session as Session
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( attribute
        , autofocus
        , css
        , disabled
        , id
        , placeholder
        , src
        , type_
        , value
        )
import Html.Styled.Events exposing (on, onClick, onInput, onSubmit)
import Html.Styled.Keyed as Keyed
import Http
import Json.Decode as Decode
import Markdown
import Specification exposing (colors)
import Task
import Time


port onSelectFile : String -> Cmd msg



-- MODEL


type alias Model =
    { session : Session.Data
    , title : String
    , body : String
    , files : List File
    }


init : Session.Data -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , title = ""
      , body = ""
      , files = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OnTitleInput String
    | OnBodyInput String
    | SaveResource
    | GotSaveResource (Result Http.Error ())
    | OnSelectFile String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTitleInput input ->
            ( { model | title = input }, Cmd.none )

        OnBodyInput input ->
            ( { model | body = input }, Cmd.none )

        OnSelectFile id ->
            ( model, onSelectFile id )

        SaveResource ->
            ( model
            , Task.attempt GotSaveResource <|
                Request.postNewResource model.title model.body model.files
            )

        GotSaveResource _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        disable =
            String.isEmpty model.title || String.isEmpty model.body
    in
    Html.Styled.form [ onSubmit SaveResource, css styling.newResource ]
        [ input
            [ onInput OnTitleInput
            , placeholder "New resource"
            , value model.title
            ]
            []
        , textarea
            [ onInput OnBodyInput
            , value model.body
            ]
            []
        , input
            [ type_ "file"
            , attribute "multiple" "true"
            , id "file-input"
            , on "change" <|
                Decode.succeed (OnSelectFile "file-input")
            ]
            []
        , div [] (List.map (\{ filename } -> img [ src filename ] []) model.files)
        , button [ disabled disable ] [ text "Save" ]
        ]


styling =
    { newResource =
        [ padding (rem 1)
        , backgroundColor colors.white
        , marginTop (rem 0.5)
        , boxShadow4 (rem 0.5) (rem 0.5) zero colors.lightGrey
        , marginBottom (rem 2)
        , Global.descendants
            [ Global.button
                [ Css.disabled
                    [ color colors.greyLighter
                    , cursor notAllowed
                    ]
                ]
            ]
        ]
    }
