module Todo exposing (Content, Id, Model)

import IdX
import Random


type alias Id =
    String


type alias Content =
    String


type alias Model =
    { id : Id
    , content : Content
    , done : Bool
    }


init id =
    { id = id, content = "", done = False }


generator =
    Random.map init IdX.stringIdGenerator
