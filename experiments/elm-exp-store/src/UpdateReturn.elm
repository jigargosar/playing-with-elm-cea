module UpdateReturn exposing (andThen, andThen3, foldlOutMsgList, pure, pure3)

import Update3


pure model =
    ( model, Cmd.none )


pure3 model =
    ( model, Cmd.none, [] )


andThen f ( m1, c1 ) =
    let
        ( m2, c2 ) =
            f m1
    in
    ( m2, Cmd.batch [ c1, c2 ] )


andThen3 f ( m1, c1, o1 ) =
    let
        ( m2, c2, o2 ) =
            f m1
    in
    ( m2, Cmd.batch [ c1, c2 ], o1 ++ o2 )


foldlOutMsgList outMsgHandler =
    Update3.eval
        (\outMsgList model ->
            outMsgList
                |> List.foldl
                    (\o1 ( m, c1 ) ->
                        outMsgHandler o1 m
                            |> Tuple.mapSecond (\c2 -> Cmd.batch [ c1, c2 ])
                    )
                    ( model, Cmd.none )
        )
