module Main exposing (main)

--import Style exposing (Transform(..), Unit(..))
--import Todo exposing (Todo)
--import Todos exposing (Todos)

import BasicsX exposing (flip, ter)
import Browser
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
import Random
import Step exposing (Step)
import Store
import Store.Item
import Task
import Todo exposing (Todo)
import TodoStore exposing (TodoStore)
import UI exposing (..)
import WheelEvent exposing (WheelEvent)



---- MODEL ----


type alias Model =
    { magicMenu : MagicMenu
    , todoStore : TodoStore
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
      }
    , Cmd.none
    )



--setTodoC : Todos -> Model -> Model
--setTodoC todos model =
--    { model | todos = todos }
---- UPDATE ----


type Msg
    = NoOp
    | MagicMenuMsg MagicMenu.Msg



--    | TodosMsg Todos.Msg


update : Msg -> Model -> Step Model Msg a
update message model =
    case message of
        NoOp ->
            Step.to model

        MagicMenuMsg msg ->
            MagicMenu.update msg model.magicMenu
                |> Step.within (\w -> { model | magicMenu = w }) MagicMenuMsg



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
            [ button [] [ text "add" ]
            , viewTodoList model
            ]

        --        , Html.map TodosMsg <|
        --            div [ class "w-100 flex flex-column justify-center items-center vs3 pv3" ]
        --                [ Todos.view model.todos ]
        --        , div [ class "w-100 flex flex-column justify-center items-center" ]
        --            [ MagicMenu.view mockActions MagicMenuMsg model.magicMenu ]
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


viewTodoItem : Store.Id -> Store.Item Todo -> Html Msg
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
