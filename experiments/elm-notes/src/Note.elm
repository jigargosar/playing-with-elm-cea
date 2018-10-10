module Note exposing (..)

import Id
import Random


type alias Note =
    { content : String, id : String }


title =
    .content


init content =
    Note content


setContent content note =
    { note | content = content }


generator : String -> Random.Generator Note
generator content =
    Random.map (Note content) Id.generator
