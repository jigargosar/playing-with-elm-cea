module EditableInput exposing (EditableInput, init, dirty, set, get, save)


type alias Record a =
    { initialValue : a, currentValue : a }


type EditableInput a
    = EditableInput (Record a)


init value =
    Record value value |> EditableInput


current =
    unTag >> .currentValue


save =
    updateRecord (\r -> { r | initialValue = r.currentValue })


get =
    current


initial =
    unTag >> .initialValue


unTag (EditableInput rec) =
    rec


updateRecord f =
    unTag >> f >> EditableInput


dirty ei =
    initial ei /= current ei


set newValue =
    always newValue |> map


map f =
    updateRecord (\r -> { r | currentValue = f r.currentValue })
