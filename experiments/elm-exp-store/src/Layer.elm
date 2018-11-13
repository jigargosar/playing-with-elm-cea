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


updateTodoDialog msg todoDialog_ =
    let
        ( todoDialog, cmd, maybeOutMsg ) =
            TodoDialog.update msg todoDialog_

        handleOut out =
            (case out of
                TodoDialog.Submit TodoDialog.Create content contextId ->
                    withOutMsg (TodoStoreMsg <| TodoStore.addNew content contextId)

                TodoDialog.Submit (TodoDialog.Edit todo) content contextId ->
                    withOutMsg (TodoStoreMsg <| TodoStore.setContentAndContextId todo.id content contextId)

                TodoDialog.Cancel ->
                    withNoOutMsg
            )
                << mapModel (\_ -> NoLayer)
    in
    mapModel (\_ -> TodoDialog todoDialog)
        >> addTaggedCmd TodoDialogMsg cmd
        >> unwrapMaybe withNoOutMsg handleOut maybeOutMsg


updateContextDialog msg contextDialog_ =
    let
        ( contextDialog, cmd, maybeOutMsg ) =
            ContextDialog.update msg contextDialog_

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
    mapModel (setLayer <| ContextDialog contextDialog)
        >> addTaggedCmd ContextDialogMsg cmd
        >> unwrapMaybe withNoOutMsg handleOut maybeOutMsg


updateContextPopup msg contextPopup_ =
    let
        ( contextPopup, cmd, maybeOutMsg ) =
            ContextPopup.update msg contextPopup_

        handleOut =
            unwrapMaybe
                withNoOutMsg
                (\out ->
                    (case out of
                        ContextPopup.ActionOut context ContextPopup.Rename ->
                            updateContextDialog ContextDialog.autoFocus <| ContextDialog.initEdit context

                        ContextPopup.ActionOut context ContextPopup.ToggleArchive ->
                            withOutMsg (ContextStoreMsg <| ContextStore.toggleArchived context.id)

                        ContextPopup.ClosedOut ->
                            withNoOutMsg
                    )
                        << mapModel (\_ -> NoLayer)
                )
                maybeOutMsg
    in
    mapModel (setLayer <| ContextPopup contextPopup)
        >> addTaggedCmd ContextPopupMsg cmd
        >> handleOut


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
