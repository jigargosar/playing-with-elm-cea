module MagicMenu exposing (MagicMenu, initial)


type alias MagicMenu =
    { open : Bool, hidden : Bool }


initial =
    MagicMenu False False


setOpen open model =
    { model | open = open }


setHidden hidden model =
    { model | hidden = hidden }
