module BasicX exposing (flip, ter)


ter b t f =
    if b then
        t

    else
        f


flip fn a b =
    fn b a
