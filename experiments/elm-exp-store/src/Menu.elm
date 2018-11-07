module Menu exposing (render)

import Css exposing (..)
import DomEvents exposing (..)
import Html.Styled as Html exposing (Attribute, Html, div, styled)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Keyed exposing (node)
import Styles exposing (..)


type alias Model =
    { open : Bool
    }


init =
    Model False


type Msg child
    = NoOp
    | ChildSelected child


type alias Config msg child =
    { toMsg : Msg child -> msg
    , selected : child -> msg
    }


update : Config msg child -> Msg child -> Model -> ( Model, Cmd msg )
update config message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


type alias ViewConfig child msg =
    { config : Config msg child
    , state : Model
    , domId : DomId
    , children : List child
    , containerStyles : List Css.Style
    , childContent : child -> List (Html msg)
    }


render : ViewConfig child msg -> Html (Msg child)
render vConfig =
    styled div
        ([ bg "white"
         , elevation 4
         , borderRadius (rem 0.5)
         ]
            ++ vConfig.containerStyles
        )
        [ id vConfig.domId ]
        (vConfig.children
            |> List.map
                (\child ->
                    let
                        childContent =
                            vConfig.childContent child
                    in
                    div [ onClick <| ChildSelected child ] childContent
                )
        )



--        |> Html.map vConfig.config.toMsg
