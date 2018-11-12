module Layer exposing (Layer(..), eqContextPopupFor)

import ContextDialog
import ContextPopup
import ContextStore exposing (Context, ContextId, ContextStore)
import TodoDialog


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
