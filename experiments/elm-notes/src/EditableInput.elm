module EditableInput exposing (EditableInput, init, dirty, set, get, save)


type alias Model a =
    { initial : a
    , current : a
    }


type alias EditableInput a =
    Model a


init value =
    Model value value


save r =
    { r | initial = r.current }


get =
    .current


dirty ei =
    ei.initial /= ei.current


set newValue =
    always newValue |> map


map f r =
    { r | current = f r.current }
