module UpdateReturn exposing
    ( Update3Config
    , addCmd
    , addCmd3
    , addOutMsg3
    , andThen
    , andThen3
    , foldlOutMsgList
    , generate3
    , perform
    , perform3
    , performWithNow
    , pure
    , pure3
    , update3
    )

import Random
import Task
import Time
import Update3


pure model =
    ( model, Cmd.none )


addCmd c2 ( m, c1 ) =
    ( m, Cmd.batch [ c1, c2 ] )


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


addOutMsg3 o2 ( m1, c1, o1 ) =
    ( m1, c1, o1 ++ [ o2 ] )


addCmd3 c2 ( m1, c1, o1 ) =
    ( m1, Cmd.batch [ c1, c2 ], o1 )


perform3 toMsg =
    Task.perform toMsg >> addCmd3


perform toMsg =
    Task.perform toMsg >> addCmd


performWithNow toMsg =
    Time.now |> Task.map Time.posixToMillis |> perform toMsg


generate3 toMsg =
    Random.generate toMsg >> addCmd3


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


type alias Update3Config small smallMsg outMsg big bigMsg =
    { get : big -> small
    , set : small -> big -> big
    , toMsg : smallMsg -> bigMsg
    , update : smallMsg -> small -> ( small, Cmd smallMsg, List outMsg )
    , toOutMsg : outMsg -> bigMsg
    , updateOutMsg : bigMsg -> big -> ( big, Cmd bigMsg )
    }


update3 : Update3Config small smallMsg outMsg big bigMsg -> smallMsg -> big -> ( big, Cmd bigMsg )
update3 config smallMsg bigModel =
    Update3.lift
        config.get
        config.set
        config.toMsg
        config.update
        smallMsg
        bigModel
        |> foldlOutMsgList (config.updateOutMsg << config.toOutMsg)
