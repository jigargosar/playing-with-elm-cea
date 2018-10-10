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


setContent now content note =
    { note | content = content, modifiedAt = now }


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


tsDecoder now =
    D.int
        |> D.map
            (\ts ->
                if ts == 0 then
                    now
                else
                    ts
            )


decoder : Int -> Decoder Note
decoder now =
    D.map4 Note
        (D.field "content" D.string)
        (D.field "createdAt" (tsDecoder now))
        (D.field "modifiedAt" (tsDecoder now))
        (D.field "id" D.string)
