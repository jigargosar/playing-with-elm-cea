module Href exposing (editNoteId, home, newNote, viewNoteId)

import Url.Builder exposing (string)


newNote =
    Url.Builder.absolute [ "notes", "new" ] []


editNoteId id =
    Url.Builder.absolute [ "notes", id ] [ string "edit" "" ]


viewNoteId id =
    Url.Builder.absolute [ "notes", id ] []


home =
    Url.Builder.absolute [] []
