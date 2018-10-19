module NotesCollection exposing (..)

import Collection exposing (Collection)
import Json.Decode as D
import Json.Encode as E
import Note exposing (Note)
import Ports
import Random
import Set exposing (Set)
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


updateNoteContent : Note.Id -> Note.Content -> NotesCollection -> Task x ( NotesCollection, Cmd msg )
updateNoteContent id content =
    Collection.updateWith id (Note.updateContent content)
        >> Task.map (\nc -> ( nc, Cmd.batch [ toPersistNoteCmdById id nc, toCacheNCCmd nc ] ))


toCacheNCCmd nc =
    Ports.cacheNotesCollection (Collection.encode Note.encode nc)


toPersistNoteCmdById id =
    Collection.get id >> Maybe.map toPersistNoteCmd >> Maybe.withDefault Cmd.none


toPersistNoteListCmdByIdSet : Collection.Ids -> NotesCollection -> Cmd msg
toPersistNoteListCmdByIdSet idSet =
    Collection.getByIdSet idSet >> toPersistNoteListCmd


toPersistNoteCmd : Note -> Cmd msg
toPersistNoteCmd =
    List.singleton >> toPersistNoteListCmd


toPersistNoteListCmd : List Note -> Cmd msg
toPersistNoteListCmd =
    E.list Note.encode >> Ports.persistNoteList


deleteAllWithIds : Set Note.Id -> NotesCollection -> Task x ( NotesCollection, Cmd msg )
deleteAllWithIds idSet =
    Collection.updateIdSetWith idSet Note.delete
        >> Task.map (\nc -> ( nc, Cmd.batch [ toPersistNoteListCmdByIdSet idSet nc, toCacheNCCmd nc ] ))
