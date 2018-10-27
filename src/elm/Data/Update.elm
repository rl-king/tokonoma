module Data.Update exposing (map)


map : (b -> c) -> (a -> msg) -> ( b, Cmd a ) -> ( c, Cmd msg )
map fn1 fn2 ( model, msg ) =
    ( fn1 model, Cmd.map fn2 msg )
