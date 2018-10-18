module Href exposing (..)

import Url.Builder exposing (string)


newNote =
    Url.Builder.absolute [ "note", "new" ] []


editNote id =
    Url.Builder.absolute [ "note", id ] [ string "edit" "" ]
