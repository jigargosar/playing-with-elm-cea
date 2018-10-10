module Note exposing (..)

import Id
import Random
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias Note =
    { content : String, createdAt : Int, modifiedAt : Int, id : String }


title =
    .content


init content =
    Note content


setContent content note =
    { note | content = content }


generator : Int -> String -> Random.Generator Note
generator now content =
    Random.map (Note content now now) Id.generator


encode note =
    E.object
        [ ( "id", E.string note.id )
        , ( "content", E.string note.content )
        , ( "createdAt", E.int note.createdAt )
        , ( "modifiedAt", E.int note.modifiedAt )
        ]


decoder : Decoder Note
decoder =
    D.map4 Note
        (D.field "content" D.string)
        --        (D.succeed 0)
        --        (D.succeed 0)
        (D.field "createdAt" D.int)
        (D.field "modifiedAt" D.int)
        (D.field "id" D.string)
