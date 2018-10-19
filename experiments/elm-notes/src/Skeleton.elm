module Skeleton exposing (AuthDetails, Details, view)

import Auth exposing (AuthState)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Details msg =
    { title : String
    , attrs : List (Html.Attribute msg)
    , kids : List (Html msg)
    }


type alias AuthDetails msg =
    { authState : AuthState
    , toAuthMsg : Auth.Msg -> msg
    }


view : AuthDetails msg -> (a -> msg) -> Details a -> Browser.Document msg
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

        photoUrl =
            Maybe.withDefault "anon.svg" maybePhotoUrl
    in
    row "pointer"
        onClickAttr
        [ txt textContent
        , img [ width 24, height 24, class "br-pill", src photoUrl ] []
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
