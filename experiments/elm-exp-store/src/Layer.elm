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
    | ContextPopup Context ContextPopup.Model
    | NoLayer


eqContextPopupFor cid layer =
    case layer of
        ContextPopup context contextPopup ->
            cid == context.id

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
    | OpenContextPopup Context


type OutMsg
    = TodoDialogOutMsg TodoDialog.OutMsg
    | ContextDialogOutMsg ContextDialog.OutMsg
    | ContextPopupOutMsg Context ContextPopup.OutMsg
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

        ( ContextPopupMsg msg, ContextPopup context model ) ->
            updateContextPopup msg context model

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

        ( OpenContextPopup context, NoLayer ) ->
            updateContextPopup ContextPopup.open context ContextPopup.init

        _ ->
            addCmd (logCmd [ "invalid msg,layer combination" ])
                >> withNoOutMsg
    )
        << pure


updateTodoDialog msg todoDialog_ =
    let
        ( todoDialog, cmd, maybeOutMsg ) =
            TodoDialog.update msg todoDialog_

        handleOut =
            unwrapMaybe
                withNoOutMsg
                (withOutMsg << TodoDialogOutMsg)
                maybeOutMsg
    in
    mapModel (\_ -> TodoDialog todoDialog)
        >> addTaggedCmd TodoDialogMsg cmd
        >> handleOut


updateContextDialog msg contextDialog_ =
    let
        ( contextDialog, cmd, maybeOutMsg ) =
            ContextDialog.update msg contextDialog_

        handleOut =
            unwrapMaybe
                withNoOutMsg
                (withOutMsg << ContextDialogOutMsg)
                maybeOutMsg
    in
    mapModel (setLayer <| ContextDialog contextDialog)
        >> addTaggedCmd ContextDialogMsg cmd
        >> handleOut


updateContextPopup msg context contextPopup_ =
    let
        ( contextPopup, cmd, maybeOutMsg ) =
            ContextPopup.update context.id msg contextPopup_

        handleOut =
            unwrapMaybe
                withNoOutMsg
                (withOutMsg << ContextPopupOutMsg context)
                maybeOutMsg
    in
    mapModel (setLayer <| ContextPopup context contextPopup)
        >> addTaggedCmd ContextPopupMsg cmd
        >> handleOut


viewLayer : { x | layer : Layer, contextStore : ContextStore } -> Html Msg
viewLayer { layer, contextStore } =
    case layer of
        ContextPopup context contextPopup ->
            ContextPopup.view context contextPopup |> Html.map ContextPopupMsg

        TodoDialog dialogModel ->
            TodoDialog.view contextStore dialogModel |> Html.map TodoDialogMsg

        ContextDialog dialogModel ->
            ContextDialog.view dialogModel |> Html.map ContextDialogMsg

        NoLayer ->
            noHtml
