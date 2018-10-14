module HotKey exposing (..)

import Exts.List
import Html.Events
import Json.Decode
import Keyboard.Event
import Keyboard.Key


anySoftKeyDown : Keyboard.Event.KeyboardEvent -> Bool
anySoftKeyDown { shiftKey, metaKey, ctrlKey, altKey } =
    shiftKey || metaKey || ctrlKey || altKey


noSoftKeyDown =
    not << anySoftKeyDown


onlyMetaKeyDown { shiftKey, metaKey, ctrlKey, altKey } =
    metaKey && not (shiftKey || ctrlKey || altKey)


type alias HotKey =
    { shiftKey : Bool
    , ctrlKey : Bool
    , altKey : Bool
    , metaKey : Bool
    , key : Keyboard.Key.Key
    }


defaultHotKey =
    HotKey False False False False (Keyboard.Key.Ambiguous [])


keToHotKey : Keyboard.Event.KeyboardEvent -> HotKey
keToHotKey { shiftKey, ctrlKey, altKey, metaKey, keyCode } =
    HotKey shiftKey ctrlKey altKey metaKey keyCode


hotKeyMappingHandler mapping nopMsg ke =
    mapping
        |> Exts.List.firstMatch (Tuple.first >> (==) (keToHotKey ke))
        |> Maybe.map Tuple.second
        |> Maybe.withDefault nopMsg


mappingDecoder mapping nopMsg =
    Json.Decode.map (hotKeyMappingHandler mapping nopMsg) Keyboard.Event.decodeKeyboardEvent


esc =
    { defaultHotKey | key = Keyboard.Key.Escape }


metaEnter =
    { defaultHotKey | key = Keyboard.Key.Enter, metaKey = True }


onKeyDown mapping nopMsg =
    Html.Events.on "keydown" (mappingDecoder mapping nopMsg)
