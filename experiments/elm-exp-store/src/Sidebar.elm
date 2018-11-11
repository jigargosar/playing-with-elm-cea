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
    , id : ContextId
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
            [ viewKeyedContextItem noStyle inbox ]
         , viewContextsHeader addContextClicked
         , HtmlX.keyedDiv [] <|
            List.map (viewKeyedContextItem <| Css.batch [ plRm 1 ]) active
         ]
            ++ HtmlX.emptyWhen (not << List.isEmpty) (viewArchivedWithHeader config) archived
        )


viewArchivedWithHeader { showArchived, toggleShowArchived } archived =
    [ viewArchiveBtn showArchived toggleShowArchived
    , HtmlX.when (always showArchived) viewArchivedContexts archived
    ]


viewArchivedContexts archived =
    HtmlX.keyedDiv [] <|
        List.map (viewKeyedContextItem <| Css.batch [ plRm 1 ]) archived


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
    styled listItem
        []
        []
        [ sDiv [ fa, fwb ] [] [ text "Contexts" ]
        , Btn.icon [ onClick addContextClicked ] [ Icons.plus |> Icons.default ]
        ]


listItem =
    styled div [ fa, rowCY, pRm 0.5 ]


liTextButton =
    styled button
        [ btnReset
        , hs
        , fa
        , fBody
        ]


viewKeyedContextItem style vm =
    ( vm.key, viewContextItem style vm )


viewContextItem moreStyles { name, navigateToTodoList, activeTodoCount, isSelected, moreClicked, moreOpen, cid } =
    styled listItem
        [ moreStyles, boolCss isSelected [ bc <| hsla 210 1 0.56 0.3, fwb ] ]
        [ id <| ContextPopup.getRefId cid
        , class "hide-child"
        ]
        [ liTextButton
            [ css
                [ if String.isEmpty name then
                    Css.batch [ ttl, fgGray ]

                  else
                    Css.batch [  ]
                , rowCY
                ]
            , onClick navigateToTodoList
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
                [ text <| String.fromInt activeTodoCount ]
            ]
        , Btn.sIcon
            [ fgGray
            , focus [ opacity (int 1) ]
            ]
            [ class "child"
            , if moreOpen then
                style "opacity" "1"

              else
                style "" ""
            , onClick moreClicked
            ]
            [ Icons.moreHDef ]
        ]
