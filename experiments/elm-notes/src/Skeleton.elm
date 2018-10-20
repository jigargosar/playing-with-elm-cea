module Skeleton exposing
    ( ActionConfig
    , Details
    , MBState
    , MagicMenuConfig
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
import UI exposing (fBtn)


type alias ActionConfig msg =
    { icon : FeatherIcons.Icon, msg : msg }


type alias MagicMenuConfig msg =
    { actions : List (ActionConfig msg) }


defaultMagicMenuConfig =
    { actions = [] }


type alias Details msg =
    { title : String
    , attrs : List (Html.Attribute msg)
    , kids : List (Html msg)
    , mm : MagicMenuConfig msg
    }


defaultSkeletonDetails : Details msg
defaultSkeletonDetails =
    { title = "Elm Notes"
    , attrs = []
    , kids = []
    , mm = defaultMagicMenuConfig
    }


type alias MBState =
    { open : Bool }


type alias MainDetails msg =
    { authState : AuthState
    , toAuthMsg : Auth.Msg -> msg
    , mbClickedMsg : msg
    , back : msg
    , forward : msg
    , home : msg
    , mbState : MBState
    }


view : MainDetails msg -> (a -> msg) -> Details a -> Browser.Document msg
view config toMsg details =
    { title =
        details.title
    , body =
        [ viewHeader config
        , Html.map toMsg <|
            div [ class "pa3 ph0-l h-100 w-100 overflow-y-auto flex" ]
                [ div [ class "measure-wide center w-100" ]
                    details.kids
                ]

        --        , viewFooter
        , viewMagicButton config toMsg details
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


boolHtml bool html_ =
    if bool then
        html_

    else
        text ""


viewMagicButton { mbClickedMsg, mbState, back, forward, home } toMsg details =
    let
        isOpen =
            mbState.open

        actionButtons =
            details.mm.actions
                |> List.map ((\{ icon, msg } -> UI.fBtn icon msg) >> Html.map toMsg)

        backBtn =
            UI.fBtn FeatherIcons.arrowLeft back

        forwardBtn =
            UI.fBtn FeatherIcons.arrowRight forward

        homeBtn =
            fBtn FeatherIcons.home home

        buttonRow =
            row "justify-center"
                []
                (if isOpen then
                    actionButtons

                 else
                    []
                )

        menuToggleIcon =
            if isOpen then
                FeatherIcons.x

            else
                FeatherIcons.menu
    in
    div [ class "flex flex-column absolute bottom-1 vs3" ]
        [ buttonRow
        , row ""
            []
            [ div [ class "absolute", style "left" "calc(-38px - var(--rem3) )" ] [ boolHtml isOpen homeBtn ]
            , boolHtml isOpen backBtn
            , fBtn menuToggleIcon mbClickedMsg
            , boolHtml isOpen forwardBtn
            ]
        ]



--- View Helpers


rowS3 classes attrs =
    div (class ("flex flex-row hs3 items-center " ++ classes) :: attrs)


row =
    rowS3


txtA attrs content =
    row "" attrs [ text content ]


txt =
    txtA []
