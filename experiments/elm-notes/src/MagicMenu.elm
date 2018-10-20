module MagicMenu exposing (Action, Actions, MagicMenu, Msg, Nav, view)

import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import UI exposing (boolHtml, fBtn, row)


type alias MagicMenu =
    { open : Bool }


type Msg
    = Clicked


type alias Action msg =
    { icon : FeatherIcons.Icon, msg : msg }


type alias Actions msg =
    List (Action msg)


type alias Nav msg =
    { home : msg, back : msg, forward : msg }


view : Nav msg -> (Msg -> msg) -> MagicMenu -> Html msg
view { back, forward, home } toMsg model =
    let
        isOpen =
            model.open

        backBtn =
            fBtn FeatherIcons.arrowLeft back

        forwardBtn =
            fBtn FeatherIcons.arrowRight forward

        homeBtn =
            fBtn FeatherIcons.home home

        --        buttonRow =
        --            row "justify-center"
        --                []
        --                (if isOpen then
        --                    actionButtons
        --
        --                 else
        --                    []
        --                )
        menuToggleIcon =
            if isOpen then
                FeatherIcons.x

            else
                FeatherIcons.menu
    in
    div [ class "flex flex-column absolute bottom-1 vs3" ]
        [ {- buttonRow
             ,
          -}
          row ""
            []
            [ div [ class "absolute", style "left" "calc(-38px - var(--rem3) )" ] [ boolHtml isOpen homeBtn ]
            , boolHtml isOpen backBtn
            , fBtn menuToggleIcon Clicked |> Html.map toMsg
            , boolHtml isOpen forwardBtn
            ]
        ]
