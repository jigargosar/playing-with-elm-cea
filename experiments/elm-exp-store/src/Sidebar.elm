module Sidebar exposing (Config, ContextConfig)

import ContextStore exposing (ContextId, ContextName)


type alias ContextConfig msg =
    { key : String
    , id : ContextId
    , cid : ContextId
    , name : ContextName
    , navigateToTodoList : msg
    , activeTodoCount : Int
    , isSelected : Bool
    , moreClicked : msg
    , moreOpen : Bool
    }


type alias Config msg =
    { inbox : ContextConfig msg, contexts : List (ContextConfig msg) }
