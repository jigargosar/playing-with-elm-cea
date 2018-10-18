module Skeleton exposing (..)

import Auth exposing (AuthState)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Details msg =
    { title : String
    , header : List String
    , warning : List String
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
        [ {- viewHeader details.header
             , lazy viewWarning details.warning
             ,
          -}
          viewHeader config
        , Html.map
            toMsg
          <|
            div (class "center" :: details.attrs) details.kids

        --        , viewFooter
        ]
    }


viewHeader { authState, toAuthMsg } =
    div [ class "bg-black white" ]
        [ row "b _bg-white-50 center pv3 ph3 ph0-l justify-between measure-wide shadow-1"
            []
            [ txtA [] "ELM Notes"
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
