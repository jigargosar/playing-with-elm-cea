module Layer exposing (Layer(..), Msg(..), OutMsg(..), eqContextPopupFor, update)

import BasicsX exposing (unwrapMaybe)
import ContextDialog
import ContextPopup
import ContextStore exposing (Context, ContextId, ContextStore)
import Log
import TodoDialog
import TodoStore exposing (Todo)
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


type OutMsg
    = TodoDialogOutMsg TodoDialog.OutMsg
    | ContextDialogOutMsg ContextDialog.OutMsg
    | ContextPopupOutMsg ContextPopup.OutMsg
    | NoOut


withNoOutMsg ( m, c ) =
    ( m, c, NoOut )


withOutMsg o ( m, c ) =
    ( m, c, o )


logCmd =
    Log.warn "Layer"


setLayer layer model =
    layer


update : Msg -> Layer -> ( Layer, Cmd Msg, OutMsg )
update message layer_ =
    (case ( message, layer_ ) of
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

        _ ->
            addCmd (logCmd [ "invalid msg,layer combination" ])
                >> withNoOutMsg
    )
    <|
        pure layer_


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
                (withOutMsg << ContextPopupOutMsg)
                maybeOutMsg
    in
    mapModel (setLayer <| ContextPopup context contextPopup)
        >> addTaggedCmd ContextPopupMsg cmd
        >> handleOut
