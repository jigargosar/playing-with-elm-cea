module Note exposing (..)

import Random


type alias Note =
    { content : String }


title =
    .content


init content =
    Note content


setContent content note =
    Note content


generator content =
    Random.constant (Note content)
