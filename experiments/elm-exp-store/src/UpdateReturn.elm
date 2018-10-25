module UpdateReturn exposing (addOutMsg, andThen, pure)

import Update3


pure model =
    ( model, Cmd.none )


andThen f ( m1, c1 ) =
    let
        ( m2, c2 ) =
            f m1
    in
    ( m2, Cmd.batch [ c1, c2 ] )


addOutMsg =
    Update3.addOutMsg
