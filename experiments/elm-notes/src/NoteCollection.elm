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


setDict : NoteDb -> F NoteCollection
setDict db nc =
    { nc | db = db }


setSeed seed nc =
    { nc | seed = seed }


addNew : Int -> String -> NoteCollection -> ( Note, NoteCollection )
addNew now content nc =
    let
        ( note, newSeed ) =
            Random.step (Note.generator now content) nc.seed

        newDict =
            Db.insert note.id note nc.db
    in
        ( note, setDict newDict nc |> setSeed newSeed )


type alias F a =
    a -> a


delete note nc =
    let
        newDict =
            Db.remove note.id nc.db
    in
        setDict newDict nc


updateNote : F Note -> Note -> F NoteCollection
updateNote fn note nc =
    let
        newDict =
            Db.update note.id (Maybe.map fn) nc.db
    in
        setDict newDict nc


updateNoteContent : Int -> String -> Note -> F NoteCollection
updateNoteContent now content =
    updateNote (Note.updateContent now content)


generator : E.Value -> Random.Generator NoteCollection
generator encodedNoteDb =
    let
        db : NoteDb
        db =
            D.decodeValue (decodeDb) encodedNoteDb
                |> Result.unpack
                    (Debug.log "Error" >> always (Db.empty))
                    (identity)
    in
        Random.map (NoteCollection db) Random.independentSeed


encode : NoteCollection -> E.Value
encode nc =
    DbX.encode Note.encode nc.db


decodeDb : Decoder NoteDb
decodeDb =
    DbX.decoder Note.decoder



--f : a -> E.Value
--f =
--    Elm.Kernel.Debugger.unsafeCoerce
