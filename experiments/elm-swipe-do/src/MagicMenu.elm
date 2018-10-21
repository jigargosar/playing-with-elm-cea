module MagicMenu exposing (MagicMenu, initial)


type alias MagicMenu =
    { open : Bool
    , hidden : Bool
    }


initial =
    MagicMenu False False


type Msg
    = NoOp
    | Clicked
    | Wheel
