module Sidebar exposing (Config, view)

import BasicsX exposing (..)
import Btn
import ContextPopup
import ContextStore exposing (Context, ContextId, ContextName, ContextStore)
import Css exposing (..)
import Html.Styled as Html exposing (Html, button, div, styled, text)
import Html.Styled.Attributes exposing (class, css, id, style)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed exposing (node)
import HtmlX
import Icons
import Styles exposing (..)
import TodoStore exposing (TodoStore)
import UI exposing (..)


type alias Config msg =
    { contextStore : ContextStore
    , todoStore : TodoStore
    , addContextClicked : msg
    , showArchived : Bool
    , toggleShowArchived : msg
    , isSelected : ContextId -> Bool
    , navigateToTodoList : ContextId -> msg
    , moreClicked : ContextId -> msg
    , moreOpen : ContextId -> Bool
    }


view : Config msg -> Html msg
view config =
    let
        contexts =
            ContextStore.list config.contextStore

        ( archived, active ) =
            List.partition .archived contexts
    in
    sDiv []
        [ class "min-h-100 bg-black-05" ]
        [ viewInbox config
        , viewContextsSubHeading config.addContextClicked
        , viewContexts config active
        , viewArchived config archived
        ]


getActiveTodoCount cid =
    .todoStore >> TodoStore.getActiveTodoListCountForContextId cid


viewInbox config =
    let
        cid =
            ContextStore.defaultId

        name =
            ContextStore.defaultName
    in
    listItem
        { styles = []
        , isSelected = config.isSelected cid
        , domId = ContextPopup.getRefIdFromContextId cid
        }
        [ viewContextName
            { name = name
            , onClickMsg = config.navigateToTodoList cid
            , count = getActiveTodoCount cid config
            }
        ]


viewContexts config =
    let
        viewContextItem { name, id } =
            listItem
                { styles = [ plRm 1 ]
                , isSelected = config.isSelected id
                , domId = ContextPopup.getRefIdFromContextId id
                }
                [ viewContextName
                    { name = name
                    , onClickMsg = config.navigateToTodoList id
                    , count = getActiveTodoCount id config
                    }
                , viewMoreMenuIcon { isOpen = config.moreOpen id, clickMsg = config.moreClicked id }
                ]
    in
    viewKeyedContexts viewContextItem


viewArchived config archived =
    let
        { showArchived, toggleShowArchived } =
            config
    in
    if archived |> List.isEmpty then
        noHtml

    else
        div []
            [ viewArchiveBtn showArchived toggleShowArchived
            , if showArchived then
                viewContexts config archived

              else
                noHtml
            ]


viewArchiveBtn showArchived toggleShowArchived =
    let
        toggleIcon =
            if showArchived then
                Icons.toggleRightDef

            else
                Icons.toggleLeftDef
    in
    sDiv [ pRm 1, paddingBottom (rem 0.5) ]
        []
        [ Btn.flat
            [ css [ rowCY, fontSize (rem 0.8) ]
            , onClick toggleShowArchived
            ]
            [ sDiv [ rowCXY, hs ] [] [ text "Archived" ]
            , sDiv [ rowCXY, hs ] [] [ toggleIcon ]
            ]
        ]


viewContextsSubHeading addContextClicked =
    listItem { styles = [], isSelected = False, domId = "contexts-title" }
        [ sDiv [ fa, fwb ] [] [ text "Contexts" ]
        , Btn.icon [ onClick addContextClicked ] [ Icons.plus |> Icons.default ]
        ]


listItem { styles, isSelected, domId } =
    styled div
        ([ fa, rowCY, pRm 0.5 ] ++ styles ++ [ boolCss isSelected [ bc <| hsla 210 1 0.56 0.3 ] ])
        [ id domId, class "hide-child" ]


viewMoreMenuIcon { isOpen, clickMsg } =
    Btn.sIcon
        [ fgGray
        , focus [ opacity (int 1) ]
        ]
        [ class "child"
        , if isOpen then
            style "opacity" "1"

          else
            style "" ""
        , onClick clickMsg
        ]
        [ Icons.moreHDef ]


viewContextName { name, onClickMsg, count } =
    styled button
        [ btnReset
        , hs
        , fa
        , fBody
        , rowCY
        ]
        [ css
            [ if String.isEmpty name then
                Css.batch [ ttl, fgGray ]

              else
                Css.batch []
            ]
        , onClick onClickMsg
        ]
        [ sDiv [ Css.property "word-break" "break-word" ] [] [ text <| defaultEmptyStringTo "<empty>" name ]
        , sDiv
            [ plRm 0.1
            , Css.fontSize (em 0.8)
            , Css.alignSelf Css.flexEnd
            , fwb
            , fgGray
            ]
            []
            [ text <| String.fromInt count ]
        ]


viewKeyedContexts fn =
    HtmlX.keyedDiv []
        << List.map (\context -> ( context.id, fn context ))
