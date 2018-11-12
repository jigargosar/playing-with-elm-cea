module Layer exposing (Layer(..), eqContextPopupFor)

import ContextDialog
import ContextPopup
import ContextStore exposing (Context, ContextId, ContextStore)
import TodoDialog
import UpdateReturn exposing (pure)


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


type OutMsg
    = TodoDialogOutMsg TodoDialog.OutMsg
    | ContextDialogOutMsg ContextDialog.OutMsg
    | ContextPopupOutMsg ContextPopup.OutMsg
    | NoOut


withNoOutMsg ( m, c ) =
    ( m, c, NoOut )


update : Msg -> Layer -> ( Layer, Cmd Msg, OutMsg )
update message layer =
    (case ( message, layer ) of
        ( TodoDialogMsg msg, TodoDialog model ) ->
            withNoOutMsg

        _ ->
            withNoOutMsg
    )
    <|
        pure layer
