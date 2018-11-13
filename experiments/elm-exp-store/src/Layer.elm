module Layer exposing (Layer(..), Msg(..), OutMsg(..), eqContextPopupFor, update, viewLayer)

import BasicsX exposing (unwrapMaybe)
import ContextDialog
import ContextPopup
import ContextStore exposing (Context, ContextId, ContextStore)
import Html.Styled as Html exposing (Html)
import Log
import TodoDialog
import TodoStore exposing (Todo)
import UI exposing (noHtml)
import UpdateReturn exposing (..)


type Layer
    = TodoDialog TodoDialog.Model
    | ContextDialog ContextDialog.Model
    | ContextPopup ContextPopup.Model
    | NoLayer


eqContextPopupFor cid layer =
    case layer of
        ContextPopup contextPopup ->
            ContextPopup.isOpenForContextId cid contextPopup

        _ ->
            False


type Msg
    = TodoDialogMsg TodoDialog.Msg
    | ContextDialogMsg ContextDialog.Msg
    | ContextPopupMsg ContextPopup.Msg
    | OpenCreateTodoDialog ContextId
    | OpenEditTodoDialog Todo
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


update : { x | contextStore : ContextStore, layer : Layer } -> Msg -> Layer -> ( Layer, Cmd Msg, OutMsg )
update { contextStore, layer } message =
    (case ( message, layer ) of
        ( TodoDialogMsg msg, TodoDialog model ) ->
            updateTodoDialog msg model

        ( ContextDialogMsg msg, ContextDialog model ) ->
            updateContextDialog msg model

        ( ContextPopupMsg msg, ContextPopup model ) ->
            updateContextPopup msg model

        ( OpenCreateTodoDialog cid, NoLayer ) ->
            updateTodoDialog TodoDialog.autoFocus (TodoDialog.initCreate cid)

        ( OpenEditTodoDialog todo, NoLayer ) ->
            updateTodoDialog TodoDialog.autoFocus (TodoDialog.initEdit todo)

        ( OpenCreateContextDialog, NoLayer ) ->
            updateContextDialog ContextDialog.autoFocus ContextDialog.initCreate

        ( OpenEditContextDialog cid, NoLayer ) ->
            contextStore
                |> ContextStore.get cid
                |> unwrapMaybe withNoOutMsg (updateContextDialog ContextDialog.autoFocus << ContextDialog.initEdit)

        ( OpenContextPopup cid, NoLayer ) ->
            contextStore
                |> ContextStore.get cid
                |> unwrapMaybe withNoOutMsg
                    (updateContextPopup ContextPopup.open << ContextPopup.init)

        _ ->
            let
                _ =
                    Debug.log "( message, layer )" ( message, layer )
            in
            addCmd (logCmd [ "invalid msg,layer combination" ])
                >> withNoOutMsg
    )
        << pure


updateTodoDialog msg todoDialog_ =
    let
        ( todoDialog, cmd, maybeOutMsg ) =
            TodoDialog.update msg todoDialog_

        handleOut =
            unwrapMaybe withNoOutMsg
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
    in
    mapModel (\_ -> TodoDialog todoDialog)
        >> addTaggedCmd TodoDialogMsg cmd
        >> handleOut maybeOutMsg


updateContextDialog msg contextDialog_ =
    let
        ( contextDialog, cmd, maybeOutMsg ) =
            ContextDialog.update msg contextDialog_

        handleOut =
            unwrapMaybe withNoOutMsg
                (\out ->
                    (case out of
                        ContextDialog.Submit ContextDialog.Create name ->
                            withOutMsg (ContextStoreMsg <| ContextStore.addNew name)

                        ContextDialog.Submit (ContextDialog.Edit context) name ->
                            withOutMsg (ContextStoreMsg <| ContextStore.setName context.id name)

                        ContextDialog.Cancel ->
                            withNoOutMsg
                    )
                        << mapModel (\_ -> NoLayer)
                )
                maybeOutMsg
    in
    mapModel (setLayer <| ContextDialog contextDialog)
        >> addTaggedCmd ContextDialogMsg cmd
        >> handleOut


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
