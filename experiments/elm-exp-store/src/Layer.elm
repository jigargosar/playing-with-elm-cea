module Layer exposing (Layer(..), Msg(..), OutMsg(..), eqContextPopupFor, update, viewLayer)

import BasicsX exposing (unwrapMaybe)
import ContextDialog
import ContextPopup
import ContextStore exposing (Context, ContextId, ContextStore)
import Html.Styled as Html exposing (Html)
import Log
import TodoDialog
import TodoStore exposing (Todo, TodoId, TodoStore)
import UI exposing (noHtml)
import UpdateReturn exposing (..)


type Layer
    = TodoDialog TodoDialog.Model
    | ContextDialog ContextDialog.Model
    | ContextPopup ContextPopup.Model
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
    | ContextPopupMsg ContextPopup.Msg
    | OpenCreateTodoDialog ContextId
    | OpenEditTodoDialog TodoId
    | OpenCreateContextDialog
    | OpenEditContextDialog ContextId
    | OpenContextPopup ContextId


type OutMsg
    = TodoStoreMsg TodoStore.Msg
    | ContextStoreMsg ContextStore.Msg
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


update : { x | contextStore : ContextStore, todoStore : TodoStore } -> Msg -> Layer -> ( Layer, Cmd Msg, OutMsg )
update { contextStore, todoStore } message layer =
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


updateContextPopup msg contextPopup_ =
    let
        ( contextPopup, cmd, maybeOutMsg ) =
            ContextPopup.update msg contextPopup_

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
    mapModel (setLayer <| ContextPopup contextPopup)
        >> addTaggedCmd ContextPopupMsg cmd
        >> unwrapMaybe withNoOutMsg handleOut maybeOutMsg


viewLayer : { x | layer : Layer, contextStore : ContextStore } -> Html Msg
viewLayer { layer, contextStore } =
    case layer of
        ContextPopup contextPopup ->
            ContextPopup.view contextPopup |> Html.map ContextPopupMsg

        TodoDialog todoDialog ->
            TodoDialog.view contextStore todoDialog |> Html.map TodoDialogMsg

        ContextDialog contextDialog ->
            ContextDialog.view contextDialog |> Html.map ContextDialogMsg

        NoLayer ->
            noHtml
