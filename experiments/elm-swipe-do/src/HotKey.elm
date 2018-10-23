module HotKey exposing (Event, decoder)

import BasicsX exposing (ter)
import Json.Decode as D
import Json.Encode as E


shift =
    2


alt =
    4


ctrl =
    8


meta =
    16


type alias Event =
    ( Int, String )


initEvent shift_ alt_ ctrl_ meta_ key =
    ( ter shift_ shift 0
        + ter alt_ alt 0
        + ter ctrl_ ctrl 0
        + ter meta_ meta 0
    , key
    )


decoder =
    D.map5 initEvent
        (D.field "shiftKey" D.bool)
        (D.field "altKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.field "metaKey" D.bool)
        (D.field "key" D.string)
