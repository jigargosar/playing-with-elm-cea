module Todos exposing (Msg(..), Todos, generator, update, view)

import Collection exposing (Collection)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as D
import Json.Encode as E
import Port
import Random
import Task
import Todo exposing (Todo)
import UI exposing (row, txtC)


type Mode
    = List
    | New


type alias TodoCollection =
    Collection Todo


type alias Model =
    { mode : Mode, collection : TodoCollection }


type alias Todos =
    Model


setCollection : TodoCollection -> Model -> Model
setCollection collection model =
    { model | collection = collection }


getTodoList =
    .collection >> Collection.items


generator : E.Value -> Random.Generator Model
generator enc =
    Collection.generator Todo.decoder enc
        |> Random.map (Model List)


type Msg
    = NoOp
    | NewClicked
    | NewAdded ( Todo, Collection Todo )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        NewAdded ( todo, collection ) ->
            ( setCollection collection model, Port.cacheTodoC (Collection.encode Todo.encode collection) )

        NewClicked ->
            ( model
            , Collection.createAndAdd (Todo.initWithContent "Todo XX") model.collection
                |> Task.perform NewAdded
            )


view : Model -> Html msg
view model =
    case model.mode of
        List ->
            viewList model

        New ->
            viewList model


viewList : Model -> Html msg
viewList =
    getTodoList >> List.map viewTodo >> div [ class "w-100 measure-narrow vs3" ]


viewTodo : Todo -> Html msg
viewTodo todo =
    row "" [] [ txtC "flex-auto" <| Todo.getContent todo ]
