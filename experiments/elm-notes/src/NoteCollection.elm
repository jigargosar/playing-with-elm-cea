module NoteCollection exposing (..)

import Basics.Extra exposing (flip)
import Db exposing (Db)
import DbX
import Dict
import Id exposing (Id)
import Note exposing (Note)
import Random
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Result.Extra as Result
import Result


type alias NoteDb =
    Db Note


type alias NoteCollection =
    { db : NoteDb, seed : Random.Seed }


queryAll =
    queryAllSortByModifiedAt


queryAllSortByModifiedAt =
    .db >> Db.toList >> List.map Tuple.second >> List.sortBy (.modifiedAt >> (*) -1)


setDb : NoteDb -> F NoteCollection
setDb db nc =
    { nc | db = db }


setSeed seed nc =
    { nc | seed = seed }


addNew : Int -> String -> NoteCollection -> ( Note, NoteCollection )
addNew now content nc =
    let
        ( note, newSeed ) =
            Random.step (Note.generator now content) nc.seed

        newDB =
            Db.insert note.id note nc.db
    in
        ( note, setDb newDB nc |> setSeed newSeed )


type alias F a =
    a -> a


delete note nc =
    let
        newDB =
            Db.remove note.id nc.db
    in
        setDb newDB nc


updateNote : F Note -> Note -> F NoteCollection
updateNote fn note nc =
    let
        newDB =
            Db.update note.id (Maybe.map fn) nc.db
    in
        setDb newDB nc


updateNoteContent : Int -> String -> Note -> F NoteCollection
updateNoteContent now content =
    updateNote (Note.updateContent now content)


replace : E.Value -> F NoteCollection
replace encDb nc =
    let
        newDb : NoteDb
        newDb =
            D.decodeValue (dbDecoder) encDb
                |> Result.unpack
                    (Debug.log "Error" >> always (Db.empty))
                    (identity)
    in
        setDb newDb nc


generator : E.Value -> Random.Generator NoteCollection
generator encodedNoteDb =
    let
        db : NoteDb
        db =
            D.decodeValue (dbDecoder) encodedNoteDb
                |> Result.unpack
                    (Debug.log "Error" >> always (Db.empty))
                    (identity)
    in
        Random.map (NoteCollection db) Random.independentSeed


encode : NoteCollection -> E.Value
encode nc =
    DbX.encode Note.encode nc.db


dbDecoder : Decoder NoteDb
dbDecoder =
    DbX.decoder Note.decoder



--f : a -> E.Value
--f =
--    Elm.Kernel.Debugger.unsafeCoerce
