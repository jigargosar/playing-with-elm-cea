module EditableInput exposing (..)


type EditableInput a
    = EditableInput { initialValue : a, currentValue : a }


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
