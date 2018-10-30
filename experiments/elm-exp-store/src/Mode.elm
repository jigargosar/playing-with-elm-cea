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


type OutMsg
    = TodoContentChangedOutMsg Store.Id Todo.Content


update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update message model =
    case message of
        NoOp ->
            pure3 model

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
                |> pure3

        ContentChangedInView newContent ->
            pure3 model
                |> (case model of
                        EditContentMode id _ ->
                            addOutMsg3 (TodoContentChangedOutMsg id newContent)

                        Default ->
                            identity
                   )

        EndEditMode ->
            pure3 model
