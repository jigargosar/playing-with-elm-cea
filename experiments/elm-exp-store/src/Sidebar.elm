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
        { inbox, contexts, addContextClicked, showArchived, toggleShowArchived } =
            config

        ( archived, active ) =
            List.partition .isArchived contexts
    in
    sDiv []
        [ class "min-h-100 bg-black-05" ]
        ([ HtmlX.keyedDiv [] <|
            [ viewKeyedContextItem [] inbox ]
         , viewContextsHeader addContextClicked
         , HtmlX.keyedDiv [] <|
            List.map (viewKeyedContextItem <| [ plRm 1 ]) active
         ]
            ++ HtmlX.emptyWhen (not << List.isEmpty) (viewArchivedWithHeader config) archived
        )


viewArchivedWithHeader { showArchived, toggleShowArchived } archived =
    [ viewArchiveBtn showArchived toggleShowArchived
    , HtmlX.when (always showArchived) viewArchivedContexts archived
    ]


viewArchivedContexts archived =
    HtmlX.keyedDiv [] <|
        List.map (viewKeyedContextItem <| [ plRm 1 ]) archived


viewArchiveBtn showArchived toggleShowArchived =
    let
        toggleIcon =
            if showArchived then
                Icons.toggleRightDef

            else
                Icons.toggleLeftDef
    in
    sDiv [ pRm 1, paddingBottom (rem 0) ]
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
    listItem { styles = [], isSelected = False }
        []
        [ sDiv [ fa, fwb ] [] [ text "Contexts" ]
        , Btn.icon [ onClick addContextClicked ] [ Icons.plus |> Icons.default ]
        ]


listItem { styles, isSelected } =
    styled div ([ fa, rowCY, pRm 0.5 ] ++ styles ++ [ boolCss isSelected [ bc <| hsla 210 1 0.56 0.3 ] ])


liTextButton =
    styled button
        [ btnReset
        , hs
        , fa
        , fBody
        ]


viewKeyedContextItem moreStyles vm =
    ( vm.key, viewContextItem moreStyles vm )


viewContextItem moreStyles { name, navigateToTodoList, activeTodoCount, isSelected, moreClicked, moreOpen, cid } =
    listItem { styles = moreStyles, isSelected = isSelected }
        [ id <| ContextPopup.getRefIdFromContextId cid
        , class "hide-child"
        ]
        [ viewContextName { name = name, onClickMsg = navigateToTodoList, count = activeTodoCount }
        , viewMoreMenuIcon { isOpen = moreOpen, clickMsg = moreClicked }
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
