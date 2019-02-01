module Page.Edit exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Navigation
import Css exposing (..)
import Css.Breakpoint as Breakpoint
import Css.Global as Global exposing (global)
import Data.FileData exposing (FileData)
import Data.Request as Request
import Data.Resource as Resource exposing (Resource)
import Data.Session as Session
import File exposing (File)
import File.Select
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( attribute
        , autofocus
        , class
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



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.none



-- MODEL


type alias Model =
    { session : Session.Data
    , title : String
    , body : String
    , files : List FileData
    , saveStatus : SaveStatus
    }


type SaveStatus
    = Unsaved
    | Saved
    | Edited
    | Error


init : Session.Data -> Maybe Int -> ( Model, Cmd Msg )
init session maybeId =
    let
        maybeGetExisting =
            case maybeId of
                Nothing ->
                    Cmd.none

                Just id ->
                    Task.attempt GotExistingResource <|
                        Request.getResource id
    in
    ( { session = session
      , title = ""
      , body = ""
      , files = []
      , saveStatus = Unsaved
      }
    , maybeGetExisting
    )



-- UPDATE


type Msg
    = OnTitleInput String
    | OnBodyInput String
    | SaveResource
    | DeleteResource
    | GotExistingResource (Result Http.Error Resource)
    | GotSaveResource (Result Http.Error Int)
    | GotDeleteResource (Result Http.Error ())
    | SelectFile
    | FilesSelected File (List File)
    | GotFileUpload (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTitleInput input ->
            ( { model | title = input }, Cmd.none )

        OnBodyInput input ->
            ( { model | body = input }, Cmd.none )

        SelectFile ->
            ( model
            , File.Select.files [ "image/png", "image/jpeg" ] FilesSelected
            )

        FilesSelected file files ->
            ( model
            , Task.attempt GotFileUpload <|
                Request.postFiles (file :: files)
            )

        GotFileUpload (Ok _) ->
            ( model, Cmd.none )

        GotFileUpload (Err _) ->
            ( model, Cmd.none )

        SaveResource ->
            ( model
            , Task.attempt GotSaveResource <|
                Request.postNewResource model.title model.body []
            )

        DeleteResource ->
            ( model
            , Task.attempt GotDeleteResource <|
                Request.deleteResource 1
            )

        GotExistingResource (Ok resource) ->
            ( { model
                | title = resource.title
                , body = resource.body
                , files = resource.files
              }
            , Cmd.none
            )

        GotExistingResource (Err _) ->
            ( model, Cmd.none )

        GotSaveResource (Ok id) ->
            ( { model | saveStatus = Saved }
            , Navigation.replaceUrl
                (Session.getNavKey model.session)
                ("/edit/" ++ String.fromInt id)
            )

        GotSaveResource (Err _) ->
            ( { model | saveStatus = Error }, Cmd.none )

        GotDeleteResource (Ok _) ->
            ( { model | saveStatus = Saved }, Cmd.none )

        GotDeleteResource (Err _) ->
            ( { model | saveStatus = Error }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ css styling.main ]
        [ viewMainEdit model
        , viewMetaData model
        ]


viewMainEdit : Model -> Html Msg
viewMainEdit model =
    section [ css styling.mainEdit ]
        [ textarea
            [ onInput OnBodyInput
            , value model.body
            ]
            []
        ]


viewMetaData : Model -> Html Msg
viewMetaData model =
    section [ css styling.metaData ]
        [ viewMetaDataStatus model
        , input
            [ onInput OnTitleInput
            , placeholder "Title"
            , value model.title
            ]
            []
        , button [ onClick SelectFile ] [ text "Upload" ]
        , viewFiles model.files
        ]


viewMetaDataStatus : Model -> Html Msg
viewMetaDataStatus model =
    div [ css styling.metaDataStatus ]
        [ button
            [ disabled (String.isEmpty model.title)
            , onClick SaveResource
            , class (saveStatusToString model.saveStatus)
            ]
            [ text (saveStatusToString model.saveStatus) ]
        ]


viewFiles : List FileData -> Html msg
viewFiles files =
    Keyed.node "div" [ css styling.files ] <|
        List.map viewFile files


viewFile : FileData -> ( String, Html msg )
viewFile { path } =
    ( path, img [ src path ] [] )



-- SAVESTATUS


saveStatusToString : SaveStatus -> String
saveStatusToString saveStatus =
    case saveStatus of
        Unsaved ->
            "Unsaved"

        Saved ->
            "Saved"

        Edited ->
            "Edited"

        Error ->
            "Error"



-- STYLING


styling =
    { main =
        [ Breakpoint.small [ displayFlex ]
        , width (pct 100)
        , padding (rem 2)
        ]
    , mainEdit =
        [ paddingRight (rem 2)
        , backgroundColor colors.white
        , marginTop (rem 0.5)
        , marginBottom (rem 2)
        , width (pct 100)
        ]
    , metaData =
        [ backgroundColor colors.white
        , marginTop (rem 0.5)
        , marginBottom (rem 2)
        , width (rem 30)
        ]
    , metaDataStatus =
        [ marginBottom (rem 1)
        , padding (rem 1)
        , backgroundColor colors.lightGrey
        , displayFlex
        , fontWeight (int 500)
        , justifyContent spaceBetween
        , alignItems center
        , Global.descendants
            [ Global.button
                [ color colors.white
                , Css.disabled
                    [ cursor notAllowed
                    ]
                , Global.withClass (saveStatusToString Unsaved)
                    [ backgroundColor colors.red ]
                , Global.withClass (saveStatusToString Saved)
                    [ backgroundColor colors.green ]
                , Global.withClass (saveStatusToString Edited)
                    [ backgroundColor colors.blue ]
                ]
            ]
        ]
    , files =
        [ Global.descendants
            [ Global.img
                [ height (rem 6)
                , width (rem 6)
                , margin4 zero (rem 1) (rem 1) zero
                , property "object-fit" "cover"
                ]
            ]
        ]
    }
