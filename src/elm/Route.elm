module Route exposing (parse)

import Url
import Url.Builder as Builder
import Url.Parser as Parser
    exposing
        ( (</>)
        , (<?>)
        , Parser
        )
import Url.Parser.Query as Query


type Route
    = Resources
    | Compose


parse : Url.Url -> Route
parse =
    let
        parser =
            Parser.oneOf
                [ Parser.map Resources Parser.top
                , Parser.map Compose (Parser.s "top" <?> Query.int "page")
                ]
    in
    Parser.parse parser
        >> Maybe.withDefault Resources
