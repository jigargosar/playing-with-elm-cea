module MagicMenu exposing (Action, MagicMenu)

import FeatherIcons


type alias MagicMenu =
    { open : Bool }


type alias Action msg =
    { icon : FeatherIcons.Icon, msg : msg }


type alias Nav msg =
    { home : msg, back : msg, forward : msg }
