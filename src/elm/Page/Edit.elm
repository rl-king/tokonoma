port module Page.Edit exposing (Model, Msg, init, subscriptions, update, view)

import Css exposing (..)
import Css.Breakpoint as Breakpoint
import Css.Global as Global exposing (global)
import Data.File as File exposing (File)
import Data.Request as Request
import Data.Resource as Resource exposing (Resource)
import Data.Session as Session
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



-- PORTS


port onFileUpload : (Decode.Value -> msg) -> Sub msg


port onSelectFile : String -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    onFileUpload <|
        GotFileUpload
            << Decode.decodeValue (Decode.list File.decode)



-- MODEL


type alias Model =
    { session : Session.Data
    , title : String
    , body : String
    , files : List File
    , saveStatus : SaveStatus
    }


type SaveStatus
    = Unsaved
    | Saved
    | Edited
    | Error


init : Session.Data -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , title = ""
      , body = ""
      , files = []
      , saveStatus = Unsaved
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OnTitleInput String
    | OnBodyInput String
    | SaveResource
    | DeleteResource
    | GotSaveResource (Result Http.Error ())
    | GotDeleteResource (Result Http.Error ())
    | GotFileUpload (Result Decode.Error (List File))
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

        DeleteResource ->
            ( model
            , Task.attempt GotDeleteResource <|
                Request.deleteResource 1
            )

        GotSaveResource (Ok _) ->
            ( { model | saveStatus = Saved }, Cmd.none )

        GotSaveResource (Err _) ->
            ( { model | saveStatus = Error }, Cmd.none )

        GotDeleteResource (Ok _) ->
            ( { model | saveStatus = Saved }, Cmd.none )

        GotDeleteResource (Err _) ->
            ( { model | saveStatus = Error }, Cmd.none )

        GotFileUpload (Ok files) ->
            ( { model | files = files ++ model.files }, Cmd.none )

        GotFileUpload (Err _) ->
            ( model, Cmd.none )



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
        , input
            [ type_ "file"
            , attribute "multiple" "true"
            , placeholder "Main content"
            , id "file-input"
            , on "change" <|
                Decode.succeed (OnSelectFile "file-input")
            ]
            []
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


viewFiles : List File -> Html msg
viewFiles files =
    Keyed.node "div" [ css styling.files ] <|
        List.map viewFile files


viewFile : File -> ( String, Html msg )
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
