module Session exposing (..)

import Browser.Navigation as Nav
import Collection
import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E
import Note exposing (Note)
import NotesCollection exposing (NotesCollection)
import Random


type alias Session =
    { key : Nav.Key, nc : NotesCollection }


generator : Nav.Key -> E.Value -> Random.Generator Session
generator key encodedNC =
    NotesCollection.generator encodedNC
        |> Random.map (Session key)


pushHref href session =
    Nav.pushUrl session.key href


replaceHref href session =
    Nav.replaceUrl session.key href


setNC nc session =
    { session | nc = nc }


overNC : (NotesCollection -> NotesCollection) -> Session -> Session
overNC updateFn model =
    { model | nc = updateFn model.nc }


getNote id =
    .nc >> Collection.get id
