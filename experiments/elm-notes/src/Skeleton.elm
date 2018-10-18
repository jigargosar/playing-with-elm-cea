module Skeleton exposing (..)

import Auth exposing (AuthState)
import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (class)


type alias Details msg =
    { title : String
    , header : List String
    , warning : List String
    , attrs : List (Html.Attribute msg)
    , kids : List (Html msg)
    }
