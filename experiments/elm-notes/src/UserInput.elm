module UserInput exposing (..)

import EditableInput
import Throttle


type alias Model a =
    { editable : EditableInput.Model a
    , throttleSave : Throttle.Model
    }


init : a -> Model a
init value =
    { editable = EditableInput.init value
    , throttleSave = Throttle.init 3000
    }
