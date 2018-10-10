module Note exposing (..)

import Id
import Random
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


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


encode note =
    E.object
        [ ( "id", E.string note.id )
        , ( "content", E.string note.content )
        ]


decoder : Decoder Note
decoder =
    D.map2 Note
        (D.field "content" D.string)
        (D.field "id" D.string)
