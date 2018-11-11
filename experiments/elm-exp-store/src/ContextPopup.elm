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
import ContextStore exposing (Context, ContextId)
import Css exposing (..)
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


getAutoFocusDomId uid =
    getChildDomId (getPopperDomId uid) 0


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
                >> addCmd (getAutoFocusDomId uniqueId |> Focus.attempt FocusResult)
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
    | ToggleArchive


actions =
    [ Rename, ToggleArchive ]


getChildText context child =
    case child of
        Rename ->
            "Rename"

        ToggleArchive ->
            if context.archived then
                "Unarchive"

            else
                "Archive"


getChildDomId popperDomId idx =
    popperDomId ++ "-" ++ String.fromInt idx


childContent idx context popperDomId child =
    [ sDiv [ p2Rm 0 0 ]
        []
        [ styled button
            [ btnReset, p2Rm 0.5 1, w100 ]
            [ id <| getChildDomId popperDomId idx
            ]
            [ text <| getChildText context child
            ]
        ]
    ]


view : Context -> Model -> Html Msg
view context model =
    case model.refEle of
        Just element ->
            viewPopup context element model

        _ ->
            noHtml


viewPopup : Context -> Element -> Model -> Html Msg
viewPopup context ref model =
    let
        uniqueId =
            context.id

        popperDomId =
            getPopperDomId uniqueId

        viewChild idx child =
            div
                [ onClick <| ActionClicked child ]
                (childContent idx context popperDomId child)

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
                (List.indexedMap viewChild actions)

        backdropAttrs =
            [ id <| getBackdropDomId uniqueId, onClickTargetId BackdropClicked ]
    in
    --    viewModalContent
    UI.backdrop backdropAttrs [ viewModalContent ]
