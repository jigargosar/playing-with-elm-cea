module Sidebar exposing (Config, ContextConfig, view)

import Btn
import ContextPopup
import ContextStore exposing (ContextId, ContextName)
import Css exposing (..)
import CssAtoms exposing (..)
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
    }


view : Config msg -> Html msg
view { inbox, contexts, addContextClicked, showArchived } =
    let
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
            ++ (if List.length archived > 0 then
                    [ viewArchiveBtn showArchived
                    , HtmlX.keyedDiv [] <|
                        List.map (viewKeyedContextItem <| Css.batch [ plRm 1 ]) archived
                    ]

                else
                    []
               )
        )


viewArchiveBtn showArchived =
    let
        textPrefix =
            if showArchived then
                "hide"

            else
                "show"
    in
    Btn.flat [ css [ rowCXY, w100, fontSize (rem 0.8) ] ] [ text <| textPrefix ++ " archived" ]


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
            [ css [ ttu, rowCY ]
            , onClick navigateToTodoList
            ]
            [ div [] [ text <| name ]
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
