module Sidebar exposing (Config, ContextConfig, view)

import BasicsX exposing (..)
import Btn
import ContextPopup
import ContextStore exposing (ContextId, ContextName)
import Css exposing (..)
import Html.Styled as Html exposing (Html, button, div, styled, text)
import Html.Styled.Attributes exposing (class, css, id, style)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed exposing (node)
import HtmlX
import Icons
import Styles exposing (..)
import UI exposing (..)


type alias ContextConfig msg =
    { key : String
    , cid : ContextId
    , name : ContextName
    , isArchived : Bool
    , navigateToTodoList : msg
    , activeTodoCount : Int
    , isSelected : Bool
    , moreClicked : msg
    , moreOpen : Bool
    }


type alias Config msg =
    { inbox : ContextConfig msg
    , contexts : List (ContextConfig msg)
    , addContextClicked : msg
    , showArchived : Bool
    , toggleShowArchived : msg
    }


view : Config msg -> Html msg
view config =
    let
        { inbox, contexts, addContextClicked } =
            config

        ( archived, active ) =
            List.partition .isArchived contexts
    in
    sDiv []
        [ class "min-h-100 bg-black-05" ]
        [ viewInboxItem inbox
        , viewContextsHeader addContextClicked
        , viewContextItems active
        , viewArchived config archived
        ]


viewInboxItem inbox =
    let
        viewContextItem { navigateToTodoList, activeTodoCount, isSelected } =
            listItem
                { styles = []
                , isSelected = isSelected
                , domId = ContextPopup.getRefIdFromContextId ContextStore.defaultId
                }
                [ viewContextName
                    { name = ContextStore.defaultName
                    , onClickMsg = navigateToTodoList
                    , count = activeTodoCount
                    }
                ]
    in
    viewKeyed viewContextItem [ inbox ]


viewContextItems =
    let
        viewContextItem { name, navigateToTodoList, activeTodoCount, isSelected, moreClicked, moreOpen, cid } =
            listItem { styles = [ plRm 1 ], isSelected = isSelected, domId = ContextPopup.getRefIdFromContextId cid }
                [ viewContextName { name = name, onClickMsg = navigateToTodoList, count = activeTodoCount }
                , viewMoreMenuIcon { isOpen = moreOpen, clickMsg = moreClicked }
                ]
    in
    viewKeyed viewContextItem


viewArchived { showArchived, toggleShowArchived } archived =
    if archived |> List.isEmpty then
        noHtml

    else
        div []
            [ viewArchiveBtn showArchived toggleShowArchived
            , HtmlX.when (always showArchived) viewContextItems archived
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


viewContextsHeader addContextClicked =
    listItem { styles = [], isSelected = False, domId = "contexts-title" }
        [ sDiv [ fa, fwb ] [] [ text "Contexts" ]
        , Btn.icon [ onClick addContextClicked ] [ Icons.plus |> Icons.default ]
        ]


listItem { styles, isSelected, domId } =
    styled div
        ([ fa, rowCY, pRm 0.5 ] ++ styles ++ [ boolCss isSelected [ bc <| hsla 210 1 0.56 0.3 ] ])
        [ id domId, class "hide-child" ]


liTextButton =
    styled button
        [ btnReset
        , hs
        , fa
        , fBody
        ]


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
    liTextButton
        [ css
            [ if String.isEmpty name then
                Css.batch [ ttl, fgGray ]

              else
                Css.batch []
            , rowCY
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


viewKeyed fn =
    HtmlX.keyedDiv []
        << List.map (\config -> ( config.key, fn config ))
