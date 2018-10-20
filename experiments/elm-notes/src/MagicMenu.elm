module MagicMenu exposing (ActionConfig, MagicMenuDetails)

import FeatherIcons


type alias ActionConfig msg =
    { icon : FeatherIcons.Icon, msg : msg }


type alias MagicMenuDetails msg =
    { actions : List (ActionConfig msg) }
