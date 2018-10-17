port module Ports exposing (..)

import Json.Decode as D
import Json.Encode as E


port persistNoteCollection : E.Value -> Cmd msg


port persistNote : E.Value -> Cmd msg


port authStateChanged : (E.Value -> msg) -> Sub msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port notesCollectionChanged : (E.Value -> msg) -> Sub msg
