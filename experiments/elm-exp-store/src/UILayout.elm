module UILayout exposing (gRow)

import Html.Styled exposing (div, styled)
import Styles exposing (..)


gRow =
    styled div [ flexRow, flexAuto ]
