module Note exposing (..)

import Id exposing (Id)
import IdX
import Random
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


encode note =
    E.object
        [ ( "id", Id.encode note.id )
        , ( "content", E.string note.content )
        , ( "deleted", E.bool note.deleted )
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


optionalOr propName propDecoder defaultValue =
    D.oneOf [ D.field propName propDecoder, D.succeed defaultValue ]


decoder : Decoder Note
decoder =
    D.map5 Note
        (D.field "content" D.string)
        (D.field "deleted" D.bool)
        (D.field "createdAt" D.int)
        (D.field "modifiedAt" D.int)
        (D.field "id" Id.decoder)


type alias Note =
    { content : String, deleted : Bool, createdAt : Int, modifiedAt : Int, id : Id }


generator : Int -> String -> Random.Generator Note
generator now content =
    Random.map (Note content False now now) IdX.generator


title =
    .content


init content =
    Note content


updateContent now content note =
    if content == note.content then
        note
    else
        { note | content = content, modifiedAt = now }


idStr =
    .id >> Id.toString
