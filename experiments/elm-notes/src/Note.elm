module Note exposing (..)

import BasicsX exposing (eq0, when)
import Collection
import Editable exposing (Editable(..))
import Exts.Maybe
import IdX
import Random
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


encode note =
    E.object
        [ ( "id", E.string note.id )
        , ( "content", E.string note.content )
        , ( "deleted", E.bool note.deleted )
        , ( "createdAt", E.int note.createdAt )
        , ( "modifiedAt", E.int note.modifiedAt )
        ]


decoder : Decoder Note
decoder =
    D.map5 Note
        (D.field "content" D.string)
        (D.field "deleted" D.bool)
        (D.field "createdAt" D.int)
        (D.field "modifiedAt" D.int)
        (D.field "id" D.string)


type alias Note =
    { content : String, deleted : Bool, createdAt : Int, modifiedAt : Int, id : Collection.Id }


init =
    initWithContent ""


initWithContent content id now =
    { content = content, deleted = False, id = id, createdAt = now, modifiedAt = now }



--generator : Int -> String -> Random.Generator Note
--generator now content =
--    Random.map (Note content False now now) IdX.stringIdGenerator


title =
    .content


getContent =
    .content


type alias EditableContent =
    Editable String


getEditableContent =
    getContent >> Editable.ReadOnly


updateContent : Content -> Collection.Millis -> Note -> Note
updateContent content now note =
    if content == note.content then
        note
    else
        { note | content = content, modifiedAt = now }


delete now note =
    { note | deleted = True, modifiedAt = now }


idStr =
    .id


type alias Content =
    String


type alias Id =
    Collection.Id
