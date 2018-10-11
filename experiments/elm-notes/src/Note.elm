module Note exposing (..)

import Id exposing (Id)
import IdX
import Random
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias Note =
    { content : String, createdAt : Int, modifiedAt : Int, id : Id }


title =
    .content


init content =
    Note content


updateContent now content note =
    if content == note.content then
        note
    else
        { note | content = content, modifiedAt = now }


generator : Int -> String -> Random.Generator Note
generator now content =
    Random.map (Note content now now) Id.generator


idStr =
    .id >> Id.toString


encode note =
    E.object
        [ ( "id", Id.encode note.id )
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


decoder : Decoder Note
decoder =
    D.map4 Note
        (D.field "content" D.string)
        (D.field "createdAt" D.int)
        (D.field "modifiedAt" D.int)
        (D.field "id" Id.decoder)
