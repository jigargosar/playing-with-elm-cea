module MagicMenu exposing (IconBtn, MagicMenu, MagicMenuDetails, defaultMagicMenuConfig)

import FeatherIcons


type alias MagicMenu =
    { open : Bool }


type alias IconBtn msg =
    { icon : FeatherIcons.Icon, msg : msg }


type alias MagicMenuDetails msg =
    { actions : List (IconBtn msg) }


defaultMagicMenuConfig =
    { actions = [] }
