module Href exposing (..)

import Url.Builder exposing (string)


newNote =
    Url.Builder.absolute [ "notes", "new" ] []


editNote id =
    Url.Builder.absolute [ "notes", id ] [ string "edit" "" ]
