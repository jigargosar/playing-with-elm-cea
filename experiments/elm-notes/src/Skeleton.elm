module Skeleton exposing (..)

import Auth exposing (AuthState)
import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (class)


type alias SkeletonConfig =
    { authState : AuthState }


type alias Details msg =
    { title : String
    , header : List String
    , warning : List String
    , attrs : List (Html.Attribute msg)
    , kids : List (Html msg)
    }


view : SkeletonConfig -> (a -> msg) -> Details a -> Browser.Document msg
view config toMsg details =
    { title =
        details.title
    , body =
        [ {- viewHeader details.header
             , lazy viewWarning details.warning
             ,
          -}
          {- viewHeader config.authState
             ,
          -}
          Html.map
            toMsg
          <|
            div (class "center" :: details.attrs) details.kids

        --        , viewFooter
        ]
    }
