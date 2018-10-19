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


replace : E.Value -> NotesCollection -> ( NotesCollection, Cmd msg )
replace encVal =
    Collection.replace Note.decoder encVal
        >> \nc -> ( nc, toCacheNCCmd nc )


addNewWithContent : Note.Content -> NotesCollection -> Task x ( ( Note, NotesCollection ), Cmd msg )
addNewWithContent content =
    Collection.createAndAdd (Note.initWithContent content)
        >> Task.map (\( note, nc ) -> ( ( note, nc ), Cmd.batch [ toPersistNoteCmd note, toCacheNCCmd nc ] ))


toCacheNCCmd nc =
    Ports.cacheNotesCollection (Collection.encode Note.encode nc)


toPersistNoteCmd =
    Note.encode >> Ports.persistNote


updateNoteContent : Note.Id -> Note.Content -> NotesCollection -> Task x ( NotesCollection, Cmd msg )
updateNoteContent id content =
    Collection.updateWith id (Note.updateContent content)
        >> Task.map (\nc -> ( nc, toCacheNCCmd nc ))
