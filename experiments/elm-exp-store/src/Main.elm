module Main exposing (main)

--import Style exposing (Transform(..), Unit(..))
--import Todo exposing (Todo)
--import Todos exposing (Todos)

import BasicsX exposing (flip, ter)
import Browser
import Browser.Dom
import Browser.Events
import Dict
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Json.Decode as D
import Json.Encode as E
import MagicMenu exposing (MagicMenu)
import Port
import Random
import Step exposing (Step)
import Store exposing (Item)
import Store.Item
import Task
import Todo exposing (TodoAttr)
import TodoStore exposing (TodoStore)
import UI exposing (..)
import WheelEvent exposing (WheelEvent)



---- MODEL ----


type Mode
    = ListTodoMode
    | NewTodoMode (Item TodoAttr)


type alias Model =
    { magicMenu : MagicMenu
    , todoStore : TodoStore
    , mode : Mode
    }


type alias Mills =
    Int


type alias Flags =
    { now : Mills, todos : E.Value }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        --        ( todos, cmd ) =
        --            Random.step (Todos.generator flags.todos) (Random.initialSeed flags.now)
        --                |> Tuple.first
        _ =
            1
    in
    ( { magicMenu = MagicMenu.initial
      , todoStore = TodoStore.load flags.todos
      , mode = ListTodoMode
      }
    , Cmd.none
    )



--setTodoC : Todos -> Model -> Model
--setTodoC todos model =
--    { model | todos = todos }
---- UPDATE ----


type Msg
    = Stay
    | MagicMenuMsg MagicMenu.Msg
    | TodoStoreMsg (Store.Msg TodoAttr)
    | AddClicked
    | ContentChanged Todo.Content



--    | TodosMsg Todos.Msg


update : Msg -> Model -> Step Model Msg a
update message model =
    case message of
        Stay ->
            Step.stay

        MagicMenuMsg msg ->
            MagicMenu.update msg model.magicMenu
                |> Step.within (\w -> { model | magicMenu = w }) MagicMenuMsg

        TodoStoreMsg msg ->
            Store.update msg model.todoStore
                |> Step.within (\w -> { model | todoStore = w }) TodoStoreMsg
                |> Step.onExit
                    (\exit ->
                        case exit of
                            Store.ExitNewCreated ( todo, todoStore ) ->
                                Step.to { model | todoStore = todoStore, mode = NewTodoMode todo }
                                    |> focusId newTodoInputDomId
                    )

        AddClicked ->
            let
                todo =
                    Todo.init ""
            in
            update (TodoStoreMsg <| Store.CreateNew todo) model

        ContentChanged content ->
            case model.mode of
                NewTodoMode todo ->
                    Step.stay

                ListTodoMode ->
                    Step.stay


focusId domId =
    Browser.Dom.focus domId
        |> Step.attempt (\_ -> Stay)



--        |> Task.attempt
--            (Result.map (always Stay)
--                >> Result.withDefault (LogWarn [ "Browser.Dom.focus", domId, "not found" ])
--            )
--        TodosMsg msg ->
--            Todos.update msg model.todos
--                |> Tuple.mapBoth (flip setTodoC model) (Cmd.map TodosMsg)
---- Subscriptions


subscriptions model =
    Sub.batch
        [ MagicMenu.subscriptions model.magicMenu |> Sub.map MagicMenuMsg

        --        , Todos.subscriptions model.todos |> Sub.map TodosMsg
        ]



---- VIEW ----
--mockActions =
--    [ FeatherIcons.home
--    , FeatherIcons.twitter
--    , FeatherIcons.scissors
--    , FeatherIcons.edit
--    , FeatherIcons.moon
--    ]
--        |> List.map (\icon -> MagicMenu.Action icon NoOp)
--        |> (::) (MagicMenu.Action FeatherIcons.trash2 (TodosMsg Todos.Reset))
--        |> (::) (MagicMenu.Action FeatherIcons.filePlus (TodosMsg Todos.NewClicked))


view : Model -> Html Msg
view model =
    UI.root
        [ viewToolbar model
        , div [ class "w-100 flex flex-column justify-center items-center vs3 pv3" ]
            [ button [ onClick AddClicked ] [ text "add" ]
            , viewTodoList model
            ]
        , viewModal model

        --        , Html.map TodosMsg <|
        --            div [ class "w-100 flex flex-column justify-center items-center vs3 pv3" ]
        --                [ Todos.view model.todos ]
        --        , div [ class "w-100 flex flex-column justify-center items-center" ]
        --            [ MagicMenu.view mockActions MagicMenuMsg model.magicMenu ]
        ]


viewModal model =
    case model.mode of
        NewTodoMode todo ->
            viewNewTodoModal todo

        ListTodoMode ->
            text ""


newTodoInputDomId =
    "new-todo-content-input"


viewNewTodoModal todo =
    div [ class "absolute absolute--fill bg-black-40 flex items-center justify-center" ]
        [ div
            [ class "bg-white br4 shadow-1 pa3 measure w-100"
            ]
            [ div [ class "w-100 flex" ]
                [ input
                    [ id newTodoInputDomId
                    , class "flex-auto pa3"
                    , value todo.attrs.content
                    , onInput ContentChanged
                    ]
                    []
                ]
            ]
        ]


viewTodoList : Model -> Html Msg
viewTodoList model =
    let
        todoList =
            model.todoStore
                |> Store.toIdItemPairList
                |> List.map (\( id, todo ) -> ( id, viewTodoItem id todo ))
    in
    Html.Keyed.node "div" [] todoList


viewTodoItem : Store.Id -> Store.Item TodoAttr -> Html Msg
viewTodoItem id todo =
    div [] [ text todo.attrs.content ]



--- Toolbar


viewToolbar model =
    UI.toolbar
        [ txtC "b pa3" "ELM Experiment Store"
        , spacer

        --        , viewTabs (Todos.getFilters model.todos |> List.map viewTab)
        , spacer
        ]



--onClickFilter =
--    Todos.SetFilter >> TodosMsg >> onClick
--
--viewTab ( active, filter, labelText ) =
--    txtA
--        [ onClickFilter filter
--        , class "pointer b ph3 pv2"
--        , classList [ ( "bw2 bb b--blue", active ) ]
--        ]
--        labelText


viewTabs =
    div [ class "flex bw2 bt b--transparent" ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = Step.asUpdateFunction update
        , subscriptions = subscriptions
        }
