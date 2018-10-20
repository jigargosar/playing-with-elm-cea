module MagicMenu exposing (ActionConfig, MBState, MagicMenuDetails)

import FeatherIcons


type alias MBState =
    { open : Bool }


type alias ActionConfig msg =
    { icon : FeatherIcons.Icon, msg : msg }


type alias MagicMenuDetails msg =
    { actions : List (ActionConfig msg) }
