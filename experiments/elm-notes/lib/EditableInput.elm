module EditableInput exposing (Model, init, dirty, set, get, save)


type alias Model a =
    { initial : a
    , current : a
    }


init value =
    Model value value


save model =
    { model | initial = model.current }


get =
    .current


dirty ei =
    ei.initial /= ei.current


set newValue =
    always newValue |> map


map f model =
    { model | current = f model.current }
