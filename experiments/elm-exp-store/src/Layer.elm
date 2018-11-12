module Layer exposing (Layer(..), eqContextPopupFor, initMaybeContextPopup)

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


initMaybeContextPopup : ContextId -> ContextStore -> Maybe Layer
initMaybeContextPopup cid contextStore =
    ContextStore.get cid contextStore
        |> Maybe.map (\context -> ContextPopup context ContextPopup.init)
