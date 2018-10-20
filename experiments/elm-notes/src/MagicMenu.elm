module MagicMenu exposing (ActionConfig, MagicMenu, MagicMenuDetails, defaultMagicMenuConfig)

import FeatherIcons


type alias MagicMenu =
    { open : Bool }


type alias ActionConfig msg =
    { icon : FeatherIcons.Icon, msg : msg }


type alias MagicMenuDetails msg =
    { actions : List (ActionConfig msg) }


defaultMagicMenuConfig =
    { actions = [] }
