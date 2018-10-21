module TodoCollection exposing (Msg(..), TodoCollection, generator, update, view)

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


type alias Model =
    Collection Todo


type alias TodoCollection =
    Model


generator : E.Value -> Random.Generator Model
generator enc =
    Collection.generator Todo.decoder enc


type Msg
    = NoOp
    | NewClicked
    | NewAdded ( Todo, TodoCollection )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        NewAdded ( todo, newModel ) ->
            ( newModel, Port.cacheTodoC (Collection.encode Todo.encode newModel) )

        NewClicked ->
            ( model
            , Collection.createAndAdd (Todo.initWithContent "Todo XX") model
                |> Task.perform NewAdded
            )


view : Model -> Html msg
view =
    Collection.items >> List.map viewTodo >> div [ class "w-100 measure-narrow vs3" ]


viewTodo : Todo -> Html msg
viewTodo todo =
    row "" [] [ txtC "flex-auto" <| Todo.getContent todo ]
