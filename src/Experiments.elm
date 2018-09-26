module Experiments exposing (ModelCmd, ModelCmdF, UpdateFunction, modelCmdAndThen)


type alias ModelCmd msg model =
    ( model, Cmd msg )


type alias ModelCmdF msg model =
    ModelCmd msg model -> ModelCmd msg model


type alias UpdateFunction msg model =
    msg -> model -> ( model, Cmd msg )


modelCmdAndThen : UpdateFunction msg model -> msg -> ModelCmdF msg model
modelCmdAndThen updateFn msg ( model, cmd ) =
    let
        ( model2, cmd2 ) =
            updateFn msg model
    in
    ( model2, Cmd.batch [ cmd, cmd2 ] )
