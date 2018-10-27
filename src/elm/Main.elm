port module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Css exposing (..)
import Css.Breakpoint as Breakpoint
import Css.Global as Global exposing (global)
import Data.File as File exposing (File)
import Data.Request as Request
import Data.Resource as Resource exposing (Resource)
import Data.User as User exposing (User)
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( attribute
        , autofocus
        , css
        , disabled
        , href
        , id
        , placeholder
        , src
        , style
        , target
        , type_
        , value
        )
import Html.Styled.Events exposing (on, onClick, onInput, onSubmit)
import Html.Styled.Keyed as Keyed
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Markdown
import Set exposing (Set)
import String.Interpolate exposing (interpolate)
import Task exposing (Task)
import Time
import Url exposing (Url)



-- PORTS


port onSelectFile : String -> Cmd msg


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
    onFileUpload
        (Response
            << FileUpload
            << File.decodeFileUpload
        )



-- MODEL


type alias Model =
    { key : Navigation.Key
    , auth : Auth
    , username : String
    , password : String
    , newResourceTitle : String
    , newResourceBody : String
    , newResourceFiles : List File
    , resources : List Resource
    }


type Auth
    = Auth User
    | Anonymous
    | Loading


type View
    = Resources (List Resource)
    | Files (List File)


init : flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ location key =
    ( { key = key
      , auth = Loading
      , username = ""
      , password = ""
      , newResourceTitle = ""
      , newResourceBody = ""
      , newResourceFiles = []
      , resources = []
      }
    , Task.attempt (Response << Auth_)
        Request.getStatus
    )



-- UPDATE


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | OnUsernameInput String
    | OnPasswordInput String
    | OnTitleInput String
    | OnBodyInput String
    | PerformLogin
    | SaveNewResource
    | PerformLogout
    | DeleteResource Int
    | OnSelectFile String
    | Response RequestResult


type RequestResult
    = Auth_ (Result Http.Error User)
    | Logout (Result Http.Error ())
    | PostResource (Result Http.Error ())
    | DeleteResource_ (Result Http.Error ())
    | AllResources (Result Http.Error (List Resource))
    | FileUpload (Result Decode.Error (List File))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlChange location ->
            ( model, Cmd.none )

        OnUrlRequest (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        OnUrlRequest (Browser.External href) ->
            ( model, Navigation.load href )

        OnUsernameInput input ->
            ( { model | username = input }, Cmd.none )

        OnPasswordInput input ->
            ( { model | password = input }, Cmd.none )

        OnTitleInput input ->
            ( { model | newResourceTitle = input }, Cmd.none )

        OnBodyInput input ->
            ( { model | newResourceBody = input }, Cmd.none )

        PerformLogin ->
            ( model
            , Task.attempt (Response << Auth_) <|
                Request.postLogin model.username model.password
            )

        SaveNewResource ->
            ( { model | newResourceTitle = "", newResourceBody = "" }
            , Task.attempt (Response << AllResources) <|
                Task.andThen (\_ -> Request.getResources) <|
                    Request.postNewResource
                        model.newResourceTitle
                        model.newResourceBody
                        model.newResourceFiles
            )

        PerformLogout ->
            ( model
            , Task.attempt (Response << Logout) Request.postLogout
            )

        DeleteResource id ->
            ( model
            , Task.attempt (Response << AllResources) <|
                Task.andThen (\_ -> Request.getResources) <|
                    Request.deleteResource id
            )

        OnSelectFile id ->
            ( model, onSelectFile id )

        Response (Auth_ (Ok user)) ->
            ( { model | auth = Auth user }
            , Task.attempt (Response << AllResources) Request.getResources
            )

        Response (Auth_ (Err err)) ->
            ( { model | auth = Anonymous }, Cmd.none )

        Response (Logout (Ok _)) ->
            ( { model | auth = Anonymous }, Cmd.none )

        Response (Logout (Err _)) ->
            ( model, Cmd.none )

        Response (PostResource _) ->
            ( model, Cmd.none )

        Response (AllResources (Ok resources)) ->
            ( { model | resources = resources }, Cmd.none )

        Response (AllResources (Err err)) ->
            ( model, Cmd.none )

        Response (DeleteResource_ _) ->
            ( model, Cmd.none )

        Response (FileUpload (Ok files)) ->
            ( { model | newResourceFiles = files ++ model.newResourceFiles }
            , Cmd.none
            )

        Response (FileUpload (Err err)) ->
            let
                _ =
                    Debug.log "File upload error" err
            in
            ( model
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Tokonoma"
    , body =
        [ toUnstyled <|
            main_ [ css styling.main ] ([ global globalStyling ] ++ viewBody model)
        ]
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    case model.auth of
        Loading ->
            []

        Anonymous ->
            viewLogin model

        Auth user ->
            viewAdmin user model


viewAdmin : User -> Model -> List (Html Msg)
viewAdmin { username } model =
    [ header [ css styling.header ]
        [ h1 [] [ text "Tokonoma" ]
        , section [ css styling.headerUser ]
            [ h4 [] [ text username ]
            , button [ onClick PerformLogout, css styling.logout ] [ text "Logout" ]
            ]
        ]
    , section [ css styling.content ]
        [ viewNewResource
            model.newResourceTitle
            model.newResourceBody
            model.newResourceFiles
        , viewResources model.resources
        ]
    ]


viewResources : List Resource -> Html Msg
viewResources resources =
    section [] <|
        List.map viewResource <|
            List.sortBy (negate << .id) resources


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
            , fromUnstyled <| Markdown.toHtml [] body
            ]
        , button [ onClick (DeleteResource id) ] [ text "delete" ]
        ]


viewLogin : Model -> List (Html Msg)
viewLogin model =
    let
        disable =
            String.isEmpty model.password
                || String.isEmpty model.username
    in
    [ section [ css styling.login ]
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
    ]


viewNewResource : String -> String -> List File -> Html Msg
viewNewResource title body files =
    let
        disable =
            String.isEmpty title || String.isEmpty body
    in
    Html.Styled.form [ onSubmit SaveNewResource, css styling.newResource ]
        [ input
            [ onInput OnTitleInput
            , placeholder "New resource"
            , value title
            ]
            []
        , textarea
            [ onInput OnBodyInput
            , value body
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
        , div [] (List.map (\{ filename } -> img [ src filename ] []) files)
        , button [ disabled disable ] [ text "Save" ]
        ]



-- LINK
-- STYLING


colors =
    { lightGrey = hex "f0f0f0"
    , grey = hex "2f2f2f"
    , greyLighter = hex "3f3f3f"
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
    , resource =
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
    , newResource =
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
    , login =
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
