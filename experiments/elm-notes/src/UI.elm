module UI exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


link url lbl =
    a [ href url ] [ text lbl ]
