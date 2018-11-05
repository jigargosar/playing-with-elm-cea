module ContextType exposing (ContextType(..))

import ContextStore exposing (Context)
import Dict


type ContextType
    = Inbox
    | UserDefined Context


list =
    ContextStore.list >> List.map UserDefined >> (::) Inbox
