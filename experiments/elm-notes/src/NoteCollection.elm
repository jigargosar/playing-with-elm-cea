module NoteCollection exposing (..)

import Note exposing (Note)
import Random
import Json.Decode as D
import Json.Encode as E


type alias NoteCollection =
    { list : List Note, seed : Random.Seed }


all =
    .list


setList list nc =
    { nc | list = list }


setSeed seed nc =
    { nc | seed = seed }


addNew content nc =
    let
        ( note, newSeed ) =
            Random.step (Note.generator content) nc.seed

        newList =
            note :: nc.list
    in
        nc
            |> setList newList
            |> setSeed newSeed


updateNoteContent content note nc =
    setList
        (List.map
            (\n ->
                if n == note then
                    Note.setContent content n
                else
                    n
            )
            nc.list
        )
        nc


generator : Random.Generator NoteCollection
generator =
    Random.map (NoteCollection []) Random.independentSeed


encode : NoteCollection -> E.Value
encode nc =
    E.object []
