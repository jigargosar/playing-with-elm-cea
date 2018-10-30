module Mode exposing (Mode(..), Msg(..), init)

import Store
import Todo exposing (TodoItem)


type Mode
    = Default
    | EditContentMode Store.Id Todo.Content


init : Mode
init =
    Default


type Msg
    = NoOp
    | ContentChangedInStore TodoItem
    | ContentChangedInView Todo.Content
    | EndEditMode
