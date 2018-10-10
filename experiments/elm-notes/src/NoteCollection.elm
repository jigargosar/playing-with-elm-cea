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


addNew content nc =
    setList ((::) (Note.init content) nc.list) nc


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
