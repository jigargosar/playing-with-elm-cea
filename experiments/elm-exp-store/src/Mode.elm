module Mode exposing (Mode(..), Msg(..), init)

import Store
import Todo exposing (TodoItem)
import UpdateReturn exposing (..)


type Mode
    = Default
    | EditContentMode Store.Id Todo.Content


type alias Model =
    Mode


init : Mode
init =
    Default


type Msg
    = NoOp
    | ContentChangedInStore TodoItem
    | ContentChangedInView Todo.Content
    | EndEditMode


editContentMode todo =
    EditContentMode todo.meta.id todo.attrs.content


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        ContentChangedInStore updatedTodo ->
            (case model of
                EditContentMode id content ->
                    if updatedTodo.meta.id == id then
                        editContentMode updatedTodo

                    else
                        model

                Default ->
                    model
            )
                |> pure

        ContentChangedInView _ ->
            ( model, Cmd.none )

        EndEditMode ->
            ( model, Cmd.none )
