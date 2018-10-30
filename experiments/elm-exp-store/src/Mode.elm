module Mode exposing (Mode(..))

import Store
import Todo


type Mode
    = Default
    | EditContentMode Store.Id Todo.Content
