module Motion exposing (..)

import Browser.Events
import Json.Decode as D
import Json.Encode as E
import Ramda exposing (F)


type Direction
    = Left
    | Right
    | Up
    | Down


isHorizontal dir =
    dir == Left || dir == Right


isVertical dir =
    dir == Up || dir == Down


type Sign
    = Positive
    | Negative


type MovementIntent
    = Horizontal Sign
    | Vertical Sign


type MovementIntents
    = MovementIntents (List MovementIntent)


type alias Key =
    String


type Msg
    = KeyDown String
    | KeyUp String


init =
    MovementIntents []


getList (MovementIntents list) =
    list


addKey : Key -> F MovementIntents
addKey key =
    getList >> \l -> key (::) l >> MovementIntents


removeKey : Key -> F MovementIntents
removeKey key mi =
    getList mi |> List.filter ((==) key) |> MovementIntents


noCmd m =
    ( m, Cmd.none )


update msg m =
    case msg of
        KeyUp key ->
            addKey key m |> noCmd

        KeyDown key ->
            removeKey key m |> noCmd


subscriptions =
    Sub.batch
        [ Browser.Events.onKeyDown (D.field "key" D.string |> KeyDown)
        , Browser.Events.onKeyUp (D.field "key" D.string |> KeyUp)
        ]
