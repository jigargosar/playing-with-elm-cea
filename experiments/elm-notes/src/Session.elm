module Session exposing (..)

import Browser.Navigation as Nav
import Collection
import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E
import Note exposing (Note)
import Random


type alias NotesCollection =
    Collection.Model Note


type alias Session =
    { key : Nav.Key, nc : NotesCollection }


generator : Nav.Key -> E.Value -> Random.Generator Session
generator key encodedNC =
    Collection.generator Note.decoder encodedNC
        |> Random.map (Session key)


pushHref href session =
    Nav.pushUrl session.key href


replaceHref href session =
    Nav.replaceUrl session.key href


setNC nc session =
    { session | nc = nc }
