module UpdateReturn exposing
    ( Update3Config
    , addCmd
    , addCmd3
    , addEffect
    , addMsg
    , addMsgEffect
    , addOutMsg3
    , addTaggedCmd
    , addTaggedEffect
    , addTaggedMsg
    , afterTimeout
    , andThen
    , andThen3
    , andThenWhen
    , attempt
    , foldlOutMsgList
    , generate3
    , mapCmd
    , mapIfElse
    , mapModel
    , mapWhen
    , maybeAddTaggedMsg
    , msgToCmd
    , nextTick
    , perform
    , perform3
    , performWithNow
    , pure
    , pure3
    ,  update3
       --    , updateMaybeSub

    , updateSub
    , withMaybeOutMsg
    , withNoOutMsg
    , withOutMsg
    )

import BasicsX exposing (unwrapMaybe)
import Process
import Random
import Task
import Time
import Update3


mapModel fn ( m, c ) =
    ( fn m, c )


andThenWhen modelPred fn ( m, c ) =
    if modelPred m then
        andThen fn ( m, c )

    else
        ( m, c )


mapWhen modelPred fn ( m, c ) =
    if modelPred m then
        fn ( m, c )

    else
        ( m, c )


mapIfElse modelPred tFn fFn ( m, c ) =
    if modelPred m then
        tFn ( m, c )

    else
        fFn ( m, c )


afterTimeout milli msg =
    Task.perform (always msg) (Process.sleep milli)


addEffect fn ( m, c ) =
    ( m, Cmd.batch [ c, fn m ] )


addTaggedEffect tagger fn ( m, c ) =
    ( m, Cmd.batch [ c, fn m |> Cmd.map tagger ] )


addMsgEffect fn ( m, c ) =
    ( m, Cmd.batch [ c, fn m |> msgToCmd ] )


addTaggedMsg tagger =
    Task.succeed >> Task.perform tagger >> addCmd


msgToCmd =
    Task.succeed >> Task.perform identity


maybeAddTaggedMsg tagger =
    unwrapMaybe identity (addTaggedMsg tagger)


updateSub :
    (subMsg -> subModel -> ( subModel, Cmd msg ))
    -> (model -> subModel)
    -> (subModel -> model -> model)
    -> subMsg
    -> model
    -> ( model, Cmd msg )
updateSub updateFn getSub setSub subMsg model =
    let
        ( sub, cmd ) =
            updateFn subMsg (getSub model)
    in
    ( setSub sub model, cmd )



--updateMaybeSub :
--    (subMsg -> subModel -> ( subModel, Cmd msg ))
--    -> (model -> Maybe subModel)
--    -> (subModel -> model -> model)
--    -> subMsg
--    -> model
--    -> ( model, Cmd msg )
--updateMaybeSub updateFn getMaybeSub setSub subMsg model =
--    unwrapMaybe ( model, Cmd.none )
--        (\sub -> updateSub updateFn (always sub) setSub subMsg model)
--        (getMaybeSub model)


mapCmd tagger ( m, c ) =
    ( m, Cmd.map tagger c )


pure model =
    ( model, Cmd.none )


withNoOutMsg ( m, c ) =
    ( m, c, Nothing )


withOutMsg fn ( m, c ) =
    ( m, c, Just <| fn m )


withMaybeOutMsg fn ( m, c ) =
    ( m, c, fn m )


addCmd c2 ( m, c1 ) =
    ( m, Cmd.batch [ c1, c2 ] )


addTaggedCmd tagger c2 ( m, c1 ) =
    ( m, Cmd.batch [ c1, Cmd.map tagger c2 ] )


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


perform toMessage =
    Task.perform toMessage >> addCmd


attempt resultToMsg =
    Task.attempt resultToMsg >> addCmd


nextTick msg =
    perform (\_ -> msg) (Process.sleep 0)


addMsg msg =
    perform identity (Task.succeed msg)


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
