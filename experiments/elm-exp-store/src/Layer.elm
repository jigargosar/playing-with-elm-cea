module Layer exposing (Layer(..), Msg(..), OutMsg(..), eqContextPopupFor, subscriptions, update, viewLayer)

import BasicsX exposing (unwrapMaybe)
import CmdDialog
import ContextDialog
import ContextPopup
import ContextStore exposing (Context, ContextId, ContextStore)
import DomX exposing (WindowSize)
import Element
import Html.Styled as Html exposing (Html, fromUnstyled)
import Log
import TodoDialog
import TodoStore exposing (Todo, TodoId, TodoStore)
import UI exposing (fromElement, noHtml)
import UpdateReturn exposing (..)


type Layer
    = TodoDialog TodoDialog.Model
    | ContextDialog ContextDialog.Model
    | ContextPopup ContextPopup.Model
    | CmdDialog CmdDialog.Model
    | NoLayer


eqContextPopupFor contextId layer =
    case layer of
        ContextPopup contextPopup ->
            ContextPopup.isOpenForContextId contextId contextPopup

        _ ->
            False


type Msg
    = TodoDialogMsg TodoDialog.Msg
    | ContextDialogMsg ContextDialog.Msg
    | CmdDialogMsg CmdDialog.Msg
    | ContextPopupMsg ContextPopup.Msg
    | OpenCreateTodoDialog ContextId
    | OpenCmdDialog
    | OpenEditTodoDialog TodoId
    | OpenCreateContextDialog
    | OpenEditContextDialog ContextId
    | OpenContextPopup ContextId


type OutMsg
    = TodoStoreMsg TodoStore.Msg
    | ContextStoreMsg ContextStore.Msg
    | GoToContextTodoListMsg ContextId
    | NoOut


withNoOutMsg ( m, c ) =
    ( m, c, NoOut )


withOutMsg o ( m, c ) =
    ( m, c, o )


logCmd =
    Log.warn "Layer"


setLayer layer model =
    layer


logInvalidLayer =
    addCmd (logCmd [ "invalid msg,layer combination" ])
        >> withNoOutMsg


withContextFromCid fn contextId contextStore =
    contextStore
        |> ContextStore.get contextId
        |> unwrapMaybe withNoOutMsg fn


withTodoFromId fn tid contextStore =
    contextStore
        |> TodoStore.get tid
        |> unwrapMaybe withNoOutMsg fn


subscriptions layer =
    Sub.batch
        [ case layer of
            CmdDialog model ->
                CmdDialog.subscriptions model |> Sub.map CmdDialogMsg

            _ ->
                Sub.none
        ]


type alias UpdateConfig x =
    { x | contextStore : ContextStore, todoStore : TodoStore, windowSize : WindowSize }


update : UpdateConfig x -> Msg -> Layer -> ( Layer, Cmd Msg, OutMsg )
update { contextStore, todoStore, windowSize } message layer =
    (case message of
        TodoDialogMsg msg ->
            case layer of
                TodoDialog model ->
                    updateTodoDialog msg model

                _ ->
                    logInvalidLayer

        ContextDialogMsg msg ->
            case layer of
                ContextDialog model ->
                    updateContextDialog msg model

                _ ->
                    logInvalidLayer

        ContextPopupMsg msg ->
            case layer of
                ContextPopup model ->
                    updateContextPopup msg model

                _ ->
                    logInvalidLayer

        CmdDialogMsg msg ->
            case layer of
                CmdDialog model ->
                    updateCmdDialog contextStore msg model

                _ ->
                    logInvalidLayer

        OpenCreateTodoDialog contextId ->
            case layer of
                NoLayer ->
                    updateTodoDialog TodoDialog.autoFocus (TodoDialog.initCreate contextId)

                _ ->
                    logInvalidLayer

        OpenEditTodoDialog todoId ->
            case layer of
                NoLayer ->
                    withTodoFromId
                        (updateTodoDialog TodoDialog.autoFocus << TodoDialog.initEdit)
                        todoId
                        todoStore

                _ ->
                    logInvalidLayer

        OpenCreateContextDialog ->
            case layer of
                NoLayer ->
                    updateContextDialog ContextDialog.autoFocus ContextDialog.initCreate

                _ ->
                    logInvalidLayer

        OpenEditContextDialog contextId ->
            case layer of
                NoLayer ->
                    withContextFromCid
                        (updateContextDialog ContextDialog.autoFocus << ContextDialog.initEdit)
                        contextId
                        contextStore

                _ ->
                    logInvalidLayer

        OpenContextPopup contextId ->
            case layer of
                NoLayer ->
                    withContextFromCid
                        (updateContextPopup ContextPopup.open << ContextPopup.init)
                        contextId
                        contextStore

                _ ->
                    logInvalidLayer

        OpenCmdDialog ->
            case layer of
                NoLayer ->
                    updateCmdDialog contextStore CmdDialog.AutoFocus CmdDialog.init

                _ ->
                    logInvalidLayer
    )
    <|
        pure layer


updateLayer updateFn layerTagger msgTagger outFn msg layerModel_ =
    let
        ( layerModel, cmd, maybeOutMsg ) =
            updateFn msg layerModel_
    in
    mapModel (\_ -> layerTagger layerModel)
        >> addTaggedCmd msgTagger cmd
        >> unwrapMaybe withNoOutMsg outFn maybeOutMsg


updateTodoDialog =
    updateLayer TodoDialog.update
        TodoDialog
        TodoDialogMsg
        (\out ->
            (case out of
                TodoDialog.Submit TodoDialog.Create content contextId ->
                    withOutMsg (TodoStoreMsg <| TodoStore.addNew content contextId)

                TodoDialog.Submit (TodoDialog.Edit todo) content contextId ->
                    withOutMsg (TodoStoreMsg <| TodoStore.setContentAndContextId todo.id content contextId)

                TodoDialog.Cancel ->
                    withNoOutMsg
            )
                << mapModel (\_ -> NoLayer)
        )


updateContextDialog =
    let
        handleOut out =
            (case out of
                ContextDialog.Submit ContextDialog.Create name ->
                    withOutMsg (ContextStoreMsg <| ContextStore.addNew name)

                ContextDialog.Submit (ContextDialog.Edit context) name ->
                    withOutMsg (ContextStoreMsg <| ContextStore.setName context.id name)

                ContextDialog.Cancel ->
                    withNoOutMsg
            )
                << mapModel (\_ -> NoLayer)
    in
    updateLayer ContextDialog.update
        ContextDialog
        ContextDialogMsg
        handleOut


updateContextPopup =
    let
        handleOut out =
            (case out of
                ContextPopup.ActionOut context ContextPopup.Rename ->
                    updateContextDialog ContextDialog.autoFocus <| ContextDialog.initEdit context

                ContextPopup.ActionOut context ContextPopup.ToggleArchive ->
                    withOutMsg (ContextStoreMsg <| ContextStore.toggleArchived context.id)

                ContextPopup.ClosedOut ->
                    withNoOutMsg
            )
                << mapModel (\_ -> NoLayer)
    in
    updateLayer ContextPopup.update
        ContextPopup
        ContextPopupMsg
        handleOut


updateCmdDialog contextStore =
    let
        handleOut out =
            (case out of
                CmdDialog.GotoContextTodoList contextId ->
                    withOutMsg <| GoToContextTodoListMsg contextId

                CmdDialog.Dismiss ->
                    withNoOutMsg
            )
                << mapModel (\_ -> NoLayer)
    in
    updateLayer (CmdDialog.update contextStore)
        CmdDialog
        CmdDialogMsg
        handleOut


viewLayer : { x | layer : Layer, contextStore : ContextStore, windowSize : WindowSize } -> Html Msg
viewLayer { layer, contextStore, windowSize } =
    case layer of
        ContextPopup contextPopup ->
            ContextPopup.view contextPopup |> Html.map ContextPopupMsg

        TodoDialog todoDialog ->
            TodoDialog.view contextStore todoDialog |> Html.map TodoDialogMsg

        ContextDialog contextDialog ->
            ContextDialog.view contextDialog |> Html.map ContextDialogMsg

        CmdDialog cmdDialog ->
            CmdDialog.view contextStore windowSize cmdDialog |> fromUnstyled |> Html.map CmdDialogMsg

        NoLayer ->
            noHtml
