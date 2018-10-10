module NoteCollection exposing (..)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Note exposing (Note)
import Random
import Json.Decode as D
import Json.Encode as E


type alias NoteDict =
    Dict String Note


type alias NoteCollection =
    { dict : NoteDict, seed : Random.Seed }


all =
    .dict >> Dict.values


setDict : NoteDict -> F NoteCollection
setDict dict nc =
    { nc | dict = dict }


setSeed seed nc =
    { nc | seed = seed }


addNew : String -> F NoteCollection
addNew content nc =
    let
        ( note, newSeed ) =
            Random.step (Note.generator content) nc.seed

        newDict =
            Dict.insert note.id note nc.dict
    in
        setDict newDict nc |> setSeed newSeed


type alias F a =
    a -> a


updateNoteContent : String -> Note -> F NoteCollection
updateNoteContent content note nc =
    let
        newDict =
            Dict.update note.id (Maybe.map <| Note.setContent content) nc.dict
    in
        setDict newDict nc


generator : Random.Generator NoteCollection
generator =
    Random.map (NoteCollection Dict.empty) Random.independentSeed


encode : NoteCollection -> E.Value
encode nc =
    E.list Note.encode <| all nc
