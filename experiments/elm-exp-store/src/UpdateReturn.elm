module UpdateReturn exposing (andThen2, pure)


pure model =
    ( model, Cmd.none )


andThen2 f ( m1, c1 ) =
    let
        ( m2, c2 ) =
            f m1
    in
    ( m2, Cmd.batch [ c1, c2 ] )
