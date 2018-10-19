module NotesCollection exposing (..)

import Collection exposing (Collection)
import Json.Decode as D
import Json.Encode as E
import Note exposing (Note)
import Ports
import Random
import Task exposing (Task)


type alias NotesCollection =
    Collection Note


generator : E.Value -> Random.Generator NotesCollection
generator encodedNC =
    Collection.generator Note.decoder encodedNC


addNewWithContent : Note.Content -> NotesCollection -> Task x ( ( Note, NotesCollection ), Cmd msg )
addNewWithContent content nc =
    Collection.createAndAdd (Note.initWithContent content) nc
        |> Task.map withCacheNoteCmd


withCacheNoteCmd ( note, nc ) =
    ( ( note, nc ), Ports.cacheNote (Note.encode note) )
