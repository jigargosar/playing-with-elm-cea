module Sidebar exposing (Config, view)

import BasicsX exposing (..)
import Btn
import ContextPopup
import ContextStore exposing (Context, ContextId, ContextName)
import Css exposing (..)
import Html.Styled as Html exposing (Html, button, div, styled, text)
import Html.Styled.Attributes exposing (class, css, id, style)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed exposing (node)
import HtmlX
import Icons
import Styles exposing (..)
import UI exposing (..)


type alias Config msg =
    { contexts : List Context
    , addContextClicked : msg
    , showArchived : Bool
    , toggleShowArchived : msg
    , isSelected : ContextId -> Bool
    , navigateToTodoList : ContextId -> msg
    , activeTodoCount : ContextId -> Int
    , moreClicked : ContextId -> msg
    , moreOpen : ContextId -> Bool
    }


view : Config msg -> Html msg
view config =
    let
        { contexts, addContextClicked } =
            config

        ( archived, active ) =
            List.partition .archived contexts
    in
    sDiv []
        [ class "min-h-100 bg-black-05" ]
        [ viewInbox config
        , viewContextsHeader addContextClicked
        , viewActive config active
        , viewArchived config archived
        ]


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
            , count = config.activeTodoCount cid
            }
        ]


viewActive config =
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
                    , count = config.activeTodoCount id
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
            , HtmlX.when (always showArchived) (viewActive config) archived
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


viewKeyedContexts fn =
    HtmlX.keyedDiv []
        << List.map (\context -> ( context.id, fn context ))
