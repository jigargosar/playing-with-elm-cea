module Href exposing (..)

import Url.Builder


newNote =
    Url.Builder.absolute [ "note", "new" ] []
