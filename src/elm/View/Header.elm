module View.Header exposing (view)

import Css exposing (..)
import Css.Breakpoint as Breakpoint
import Css.Global as Global exposing (global)
import Data.Auth as Auth exposing (Auth)
import Data.FileData as FileData exposing (FileData)
import Data.Session as Session
import Data.User as User exposing (User)
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( css
        , href
        )
import Html.Styled.Events exposing (onClick)
import Specification exposing (colors, ms)


view : msg -> Session.Data -> Html msg
view msg session =
    header [ css styling.header ]
        [ a [ href "/" ] [ h1 [] [ text "Tokonoma" ] ]
        , nav []
            [ a [ href "/" ] [ text "Overview" ]
            , a [ href "/edit" ] [ text "New" ]
            ]
        , viewUser msg (Session.getAuth session)
        ]


viewUser : msg -> Auth -> Html msg
viewUser msg auth =
    case auth of
        Auth.Auth user ->
            div [ css styling.headerUser ]
                [ text user.username
                , div []
                    [ button [ onClick msg, css styling.logout ] [ text "Logout" ]
                    ]
                ]

        _ ->
            text ""


styling =
    { header =
        [ color colors.white
        , backgroundColor colors.black
        , padding2 (rem 1) (rem 2)
        , displayFlex
        , justifyContent spaceBetween
        , alignItems center
        , Global.descendants
            [ Global.a
                [ color colors.white
                , fontSize (ms 1)
                ]
            , Global.nav
                [ width (pct 100)
                , marginLeft (rem 4)
                , Global.descendants
                    [ Global.a
                        [ marginRight (rem 2) ]
                    ]
                ]
            ]
        ]
    , headerUser =
        [ displayFlex
        , justifyContent flexEnd
        , alignItems center
        , position relative
        , fontSize (ms 1)
        , fontWeight (int 500)
        , Global.children
            [ Global.div
                [ display none
                , position absolute
                , bottom zero
                , right (rem -2)
                , transform (translateY (pct 100))
                , padding (rem 2)
                , backgroundColor colors.black
                ]
            ]
        , hover
            [ Global.children
                [ Global.div [ display block ] ]
            ]
        ]
    , logout =
        [ backgroundColor colors.darkGrey
        , color colors.red
        ]
    }
