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


onChange : msg -> a -> Model a -> ( Model a, Cmd msg )
onChange throttledSaveMsg newValue model =
    let
        ( newThrottleSave, cmd ) =
            Throttle.push throttledSaveMsg model.throttleSave
    in
        ( { model
            | editable = model.editable |> EditableInput.set newValue
            , throttleSave = newThrottleSave
          }
        , cmd
        )
