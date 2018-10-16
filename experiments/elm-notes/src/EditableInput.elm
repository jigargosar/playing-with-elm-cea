module EditableInput exposing (EditableInput, init, dirty, set)


type alias Record =
    { initialValue : a, currentValue : a }


type EditableInput a
    = EditableInput Record


init value =
    Record value value |> EditableInput


current =
    unTag >> .currentValue


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
