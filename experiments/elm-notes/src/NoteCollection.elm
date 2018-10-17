module NoteCollection exposing (..)

import Basics.Extra exposing (flip)
import Collection
import Dict
import Note exposing (Note)
import Random
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Result.Extra as Result
import Result


type alias NoteCollection =
    Collection.Model Note


queryAll =
    queryAllSortByModifiedAt


queryAllSortByModifiedAt =
    Collection.items
        >> List.filter (.deleted >> not)
        >> List.sortBy (.modifiedAt >> (*) -1)


getById =
    Collection.get


updateNote : (Note -> Note) -> Note -> NoteCollection -> NoteCollection
updateNote fn note nc =
    nc


updateNoteContent : Int -> String -> Note -> NoteCollection -> NoteCollection
updateNoteContent now content =
    updateNote (Note.updateContent now content)


replace : E.Value -> NoteCollection -> NoteCollection
replace encodedValue =
    Collection.replace Note.decoder encodedValue


generator : E.Value -> Random.Generator NoteCollection
generator encodedValue =
    Collection.generator Note.decoder encodedValue


encode =
    Collection.encode Note.encode
