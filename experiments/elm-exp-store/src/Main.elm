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
import Task
import Todo exposing (TodoAttr)
import TodoStore exposing (TodoStore)
import UI exposing (..)
import WheelEvent exposing (WheelEvent)



---- MODEL ----


type Mode
    = ListTodoMode
    | EditContentMode Store.Id Todo.Content


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
    ( { magicMenu = MagicMenu.initial
      , todoStore = TodoStore.load flags.todos
      , mode = ListTodoMode
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Stay
    | MagicMenuMsg MagicMenu.Msg
    | TSMsg (Store.Msg TodoAttr)
    | AddClicked
    | ContentChanged Todo.Content



--    | TodosMsg Todos.Msg


editContentMode ( id, todo ) =
    EditContentMode id todo.attrs.content


update : Msg -> Model -> Step Model Msg a
update message model =
    case message of
        Stay ->
            Step.stay

        MagicMenuMsg msg ->
            MagicMenu.update msg model.magicMenu
                |> Step.within (\w -> { model | magicMenu = w }) MagicMenuMsg

        TSMsg msg ->
            Store.update msg model.todoStore
                |> Step.within (\w -> { model | todoStore = w }) TSMsg
                |> Step.onExit
                    (\exit ->
                        case exit of
                            Store.ExitNewInserted ( idTodoPair, todoStore ) ->
                                Step.to { model | todoStore = todoStore, mode = editContentMode idTodoPair }
                                    |> focusId newTodoInputDomId

                            Store.ExitItemUpdated ( idTodoPair, todoStore ) ->
                                let
                                    newMode =
                                        case model.mode of
                                            EditContentMode id content ->
                                                if Tuple.first idTodoPair == id then
                                                    editContentMode idTodoPair

                                                else
                                                    model.mode

                                            ListTodoMode ->
                                                model.mode
                                in
                                Step.to { model | todoStore = todoStore, mode = newMode }
                    )

        AddClicked ->
            update (TSMsg <| Store.createAndInsert Todo.initEmpty) model

        ContentChanged newContent ->
            case model.mode of
                EditContentMode id _ ->
                    model
                        |> update (TSMsg <| Store.overItemAttrs id (Todo.setContent newContent) model.todoStore)

                ListTodoMode ->
                    Step.stay


focusId domId =
    Browser.Dom.focus domId
        |> Step.attempt (\_ -> Stay)



---- Subscriptions


subscriptions model =
    Sub.batch
        [ MagicMenu.subscriptions model.magicMenu |> Sub.map MagicMenuMsg
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
        EditContentMode id content ->
            viewNewTodoModal id content

        ListTodoMode ->
            text ""


newTodoInputDomId =
    "new-todo-content-input"


viewNewTodoModal todoId content =
    div [ class "absolute absolute--fill bg-black-40 flex items-center justify-center" ]
        [ div
            [ class "bg-white br4 shadow-1 pa3 measure w-100"
            ]
            [ div [ class "w-100 flex" ]
                [ input
                    [ id newTodoInputDomId
                    , class "flex-auto pa3"
                    , value content
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
