module Note exposing (..)


type alias Note =
    { content : String }


title =
    .content


init content =
    Note content


setContent content note =
    Note content
