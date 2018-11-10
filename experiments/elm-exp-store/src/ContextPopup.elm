module ContextPopup exposing
    ( Action(..)
    , Model
    , Msg
    , OutMsg(..)
    , getRefId
    , init
    , open
    , update
    , view
    )

import BasicsX exposing (..)
import Browser.Dom exposing (Element)
import ContextStore exposing (ContextId)
import Css exposing (..)
import CssAtoms exposing (..)
import DomX exposing (DomId, onClickTargetId, onFocusIn, onFocusOut)
import Focus exposing (FocusResult)
import Html.Styled exposing (Html, button, div, styled, text)
import Html.Styled.Attributes as HA exposing (attribute, autofocus, id, style)
import Html.Styled.Events exposing (onClick)
import HtmlX
import Log
import Styles exposing (..)
import Task
import UI exposing (..)
import UpdateReturn exposing (..)


type alias Model =
    { refEle : Maybe Element
    }


init : Model
init =
    { refEle = Nothing
    }


type alias BounceMsg =
    Maybe Msg


open =
    Open


type Msg
    = ActionClicked Action
    | Open
    | FocusResult FocusResult
    | ElementResult ElementResult
    | BackdropClicked DomId


type alias Config msg =
    { toMsg : Msg -> msg
    , selected : Action -> ContextId -> msg
    }


getPopperDomId uid =
    "context-popup-" ++ uid


getBackdropDomId =
    getPopperDomId >> (++) "-backdrop"


getRefId uid =
    "context-popup-ref" ++ uid


getMaybeAutoFocusDomId uid =
    actions |> List.head |> Maybe.map (getChildDomId (getPopperDomId uid))


type alias ElementResult =
    Result Browser.Dom.Error Element


attemptGetElement : (ElementResult -> msg) -> DomId -> Cmd msg
attemptGetElement resultToMsg domId =
    Browser.Dom.getElement domId
        |> Task.attempt resultToMsg


type OutMsg
    = ActionOut Action
    | ClosedOut


update : String -> Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update uniqueId message =
    (case message of
        FocusResult r ->
            addCmd (Log.focusResult "ContextPopup.elm" r)
                >> withNoOutMsg

        ElementResult (Err _) ->
            addCmd (Log.warn "ContextPopup.elm" [ "Element Not Found" ])
                >> withNoOutMsg

        ElementResult (Ok element) ->
            mapModel (\model -> { model | refEle = Just element })
                >> addCmd (getMaybeAutoFocusDomId uniqueId |> Focus.attemptMaybe FocusResult)
                >> withNoOutMsg

        BackdropClicked targetId ->
            if targetId == getBackdropDomId uniqueId then
                withOutMsg (\_ -> ClosedOut)

            else
                withNoOutMsg

        ActionClicked action ->
            withOutMsg (\_ -> ActionOut action)

        Open ->
            mapModel (\model -> { model | refEle = Nothing })
                >> addCmd (attemptGetElement ElementResult (getRefId uniqueId))
                >> withNoOutMsg
    )
        << pure


type Action
    = Rename
    | Archive


actions =
    [ Rename, Archive ]


getChildText child =
    case child of
        Rename ->
            "Rename"

        Archive ->
            "Archive"


getChildDomId popperDomId child =
    popperDomId ++ "-" ++ getChildText child


childContent popperDomId child =
    [ sDiv [ p2Rm 0 0 ]
        []
        [ styled button
            [ btnReset, p2Rm 0.5 1, w100 ]
            [ id <| getChildDomId popperDomId child
            ]
            [ text <| getChildText child
            ]
        ]
    ]


view : String -> Model -> Html Msg
view uniqueId model =
    case model.refEle of
        Just element ->
            viewPopup uniqueId element model

        _ ->
            noHtml


viewPopup : String -> Element -> Model -> Html Msg
viewPopup uniqueId ref model =
    let
        popperDomId =
            getPopperDomId uniqueId

        viewChild child =
            div
                [ onClick <| ActionClicked child ]
                (childContent popperDomId child)

        rootStyles =
            [ bg "white"
            , elevation 4
            , borderRadius (rem 0.5)
            , pRm 0.5
            , minWidth (rem 10)
            , position absolute
            , left (px <| ref.element.x + ref.element.width)
            , top (px <| min ref.element.y (ref.viewport.height - 100))
            ]

        rootAttributes =
            [ id popperDomId
            ]

        viewModalContent =
            sDiv rootStyles
                rootAttributes
                (List.map viewChild actions)

        backdropAttrs =
            [ id <| getBackdropDomId uniqueId, onClickTargetId BackdropClicked ]
    in
    --    viewModalContent
    UI.backdrop backdropAttrs [ viewModalContent ]
