module View.Header exposing (view)

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
        ( css
        , href
        )
import Html.Styled.Events exposing (onClick)
import Specification exposing (colors)


view : msg -> Html msg
view msg =
    header [ css styling.header ]
        [ a [ href "/" ] [ h1 [] [ text "Tokonoma" ] ]
        , section [ css styling.headerUser ]
            [ button [ onClick msg, css styling.logout ] [ text "Logout" ]
            ]
        ]


styling =
    { header =
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
    , logout =
        [ backgroundColor colors.darkGrey
        , color colors.red
        ]
    }
