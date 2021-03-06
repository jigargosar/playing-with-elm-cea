module Skeleton exposing
    ( Details
    , MainDetails
    , defaultSkeletonDetails
    , view
    )

import Auth exposing (AuthState)
import Browser
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MagicMenu exposing (MagicMenu)
import UI exposing (boolHtml, fBtn)


type alias Details a =
    { title : String
    , attrs : List (Html.Attribute a)
    , kids : List (Html a)
    , actions : List (MagicMenu.Action a)
    }


defaultSkeletonDetails : Details a
defaultSkeletonDetails =
    { title = "Elm Notes"
    , attrs = []
    , kids = []
    , actions = []
    }


type alias MainDetails msg =
    { authState : AuthState
    , toAuthMsg : Auth.Msg -> msg
    , magicMenu : MagicMenu
    , magicMenuNavActions : MagicMenu.NavActions msg
    , toMagicMenuMsg : MagicMenu.Msg -> msg
    }


view : MainDetails msg -> (a -> msg) -> Details a -> Browser.Document msg
view config toMsg details =
    { title =
        details.title
    , body =
        [ viewHeader config
        , Html.map toMsg <|
            div [ class "flex overflow-y-auto h-100 w-100" ]
                [ div [ class "measure-wide center w-100" ]
                    details.kids
                ]

        --        , viewFooter
        , viewMagicMenu config toMsg details
        ]
    }


viewHeader { authState, toAuthMsg } =
    div [ class "bg-black white w-100 " ]
        [ row "b _bg-white-50 pa3 ph0-l justify-between measure-wide center shadow-1"
            []
            [ a [ class "no-underline color-inherit bg-inherit ", href "/" ] [ txtA [] "ELM Notes" ]
            , case authState of
                Auth.Authenticated { displayName, photoUrl } ->
                    viewAuthAvatarBtn (Just Auth.SignOutClicked) (Just photoUrl) displayName

                Auth.InitialUnknown ->
                    viewAuthAvatarBtn Nothing Nothing "Loading"

                Auth.Anon ->
                    viewAuthAvatarBtn (Just Auth.SignInClicked) Nothing "Anon"
            ]
            |> Html.map toAuthMsg
        ]


viewAuthAvatarBtn maybeOnClick maybePhotoUrl textContent =
    let
        onClickAttr =
            Maybe.map (onClick >> List.singleton) maybeOnClick |> Maybe.withDefault []

        avatar =
            case maybePhotoUrl of
                Just url ->
                    img [ width 24, height 24, class "br-pill", src url ] []

                Nothing ->
                    FeatherIcons.user |> FeatherIcons.toHtml []
    in
    row "pointer"
        onClickAttr
        [ txt textContent
        , avatar
        ]


viewMagicMenu : MainDetails msg -> (a -> msg) -> Details a -> Html msg
viewMagicMenu { magicMenu, magicMenuNavActions, toMagicMenuMsg } toMsg details =
    let
        actions =
            details.actions
                |> List.map (\{ icon, msg } -> MagicMenu.Action icon (toMsg msg))
    in
    MagicMenu.view actions magicMenuNavActions toMagicMenuMsg magicMenu



--viewMagicMenu { mbClickedMsg, magicMenu, back, forward, home } toMsg details =
--    let
--        isOpen =
--            magicMenu.open
--
--        actionButtons =
--            details.actions
--                |> List.map ((\{ icon, msg } -> UI.fBtn icon msg) >> Html.map toMsg)
--
--        backBtn =
--            UI.fBtn FeatherIcons.arrowLeft back
--
--        forwardBtn =
--            UI.fBtn FeatherIcons.arrowRight forward
--
--        homeBtn =
--            fBtn FeatherIcons.home home
--
--        buttonRow =
--            row "justify-center"
--                []
--                (if isOpen then
--                    actionButtons
--
--                 else
--                    []
--                )
--
--        menuToggleIcon =
--            if isOpen then
--                FeatherIcons.x
--
--            else
--                FeatherIcons.menu
--    in
--    div [ class "flex flex-column absolute bottom-1 vs3" ]
--        [ buttonRow
--        , row ""
--            []
--            [ div [ class "absolute", style "left" "calc(-38px - var(--rem3) )" ] [ boolHtml isOpen homeBtn ]
--            , boolHtml isOpen backBtn
--            , fBtn menuToggleIcon mbClickedMsg
--            , boolHtml isOpen forwardBtn
--            ]
--        ]
--
--
--- View Helpers


rowS3 classes attrs =
    div (class ("flex flex-row hs3 items-center " ++ classes) :: attrs)


row =
    rowS3


txtA attrs content =
    row "" attrs [ text content ]


txt =
    txtA []
