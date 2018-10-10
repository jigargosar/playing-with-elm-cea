module NoteCollection exposing (..)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Note exposing (Note)
import Random
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Result.Extra as Result
import Result


type alias NoteDict =
    Dict String Note


type alias NoteCollection =
    { dict : NoteDict, seed : Random.Seed }


queryAll =
    --    .dict >> Dict.values
    queryAllSortByModifiedAt


queryAllSortByModifiedAt =
    .dict >> Dict.values >> List.sortBy (.modifiedAt >> (*) -1)


setDict : NoteDict -> F NoteCollection
setDict dict nc =
    { nc | dict = dict }


setSeed seed nc =
    { nc | seed = seed }


addNew : Int -> String -> NoteCollection -> ( Note, NoteCollection )
addNew now content nc =
    let
        ( note, newSeed ) =
            Random.step (Note.generator now content) nc.seed

        newDict =
            Dict.insert note.id note nc.dict
    in
        ( note, setDict newDict nc |> setSeed newSeed )


type alias F a =
    a -> a


updateNote : F Note -> Note -> F NoteCollection
updateNote fn note nc =
    let
        newDict =
            Dict.update note.id (Maybe.map fn) nc.dict
    in
        setDict newDict nc


updateNoteContent : Int -> String -> Note -> F NoteCollection
updateNoteContent now content =
    updateNote (Note.setContent now content)


generator : Int -> E.Value -> Random.Generator NoteCollection
generator now encodedNoteDict =
    let
        dict : NoteDict
        dict =
            D.decodeValue (decoder now) encodedNoteDict
                |> Result.unpack
                    (Debug.log "Error" >> always (Dict.empty))
                    (identity)
    in
        Random.map (NoteCollection dict) Random.independentSeed


encode : NoteCollection -> E.Value
encode nc =
    E.dict identity Note.encode <| nc.dict


decoder : Int -> Decoder NoteDict
decoder =
    D.dict << Note.decoder



--f : a -> E.Value
--f =
--    Elm.Kernel.Debugger.unsafeCoerce
