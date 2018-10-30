module TodoStore exposing (Item, Msg, OutMsg, TodoStore, insertNewWithContent, update)

import BasicsX exposing (Encoder, applyTo, flip, maybeBool)
import JsonCodec as JC exposing (Codec)
import Port
import Store exposing (Item, Store, itemAttrs)
import Todo exposing (TodoAttrs)


type alias Item =
    Store.Item TodoAttrs


type alias TodoStore =
    Store.Store TodoAttrs


type alias Msg =
    Store.Msg Todo.Msg TodoAttrs


type alias OutMsg =
    Store.OutMsg TodoAttrs


update =
    Store.update Todo.storeConfig


isWhiteSpaceString =
    String.trim >> String.isEmpty


insertNewWithContent content =
    Store.maybeInsertNew <| maybeBool (isWhiteSpaceString content |> not) (Todo.initAttrWithContent content)
