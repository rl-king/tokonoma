module Specification exposing (colors, ms)

import Css exposing (hex, rem)
import ModularScale


colors =
    { lightGrey = hex "f0f0f0"
    , grey = hex "2f2f2f"
    , greyLighter = hex "3f3f3f"
    , darkGrey = hex "1f1f1f"
    , black = hex "111"
    , white = hex "fff"
    , green = hex "7ED58D"
    , red = hex "FD3740"
    , yellow = hex "F1D027"
    , blue = hex "3CA5EA"
    , offwhite = hex "fafafa"
    , purple = hex "7F63D2"
    , pink = hex "f9b2e1"
    }


msConfig : ModularScale.Config
msConfig =
    ModularScale.config [ 1 ]
        ModularScale.MinorThird


ms : Int -> Css.Rem
ms x =
    rem (ModularScale.get msConfig x)
