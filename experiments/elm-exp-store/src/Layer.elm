module Layer exposing (Layer(..), Msg(..), OutMsg(..), eqContextPopupFor, update)

import BasicsX exposing (unwrapMaybe)
import ContextDialog
import ContextPopup
import ContextStore exposing (Context, ContextId, ContextStore)
import Log
import TodoDialog
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


update : Msg -> Layer -> ( Layer, Cmd Msg, OutMsg )
update message layer_ =
    (case ( message, layer_ ) of
        ( TodoDialogMsg msg, TodoDialog model ) ->
            updateTodoDialog msg model

        ( OpenCreateTodoDialog cid, NoLayer ) ->
            updateTodoDialog TodoDialog.autoFocus (TodoDialog.initCreate cid)

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
    pure
        >> mapModel (\_ -> TodoDialog todoDialog)
        >> addTaggedCmd TodoDialogMsg cmd
        >> handleOut
