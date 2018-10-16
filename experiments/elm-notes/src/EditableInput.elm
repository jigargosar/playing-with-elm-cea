module EditableInput exposing (EditableInput, init, dirty, set, get, save)


type alias Model a =
    { initial : a
    , current : a
    }


type EditableInput a
    = EditableInput (Model a)


init value =
    Model value value |> EditableInput


current =
    unTag >> .current


save =
    updateRecord (\r -> { r | initial = r.current })


get =
    current


initial =
    unTag >> .initial


unTag (EditableInput rec) =
    rec


updateRecord f =
    unTag >> f >> EditableInput


dirty ei =
    initial ei /= current ei


set newValue =
    always newValue |> map


map f =
    updateRecord (\r -> { r | current = f r.current })
