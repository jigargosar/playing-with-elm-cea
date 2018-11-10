module ContextPopup exposing
    ( Action(..)
    , Model
    , Msg
    , OutMsg
    , OutType(..)
    , init
    , isOpenForContextId
    , open
    , refIdFromCid
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
    { cid : ContextId
    , refEle : Maybe Element
    }


init : ContextId -> Model
init cid =
    { cid = cid
    , refEle = Nothing
    }


isOpenForContextId cid model =
    model.cid == cid


type alias BounceMsg =
    Maybe Msg


open =
    Open


type Msg
    = ActionClicked Action
    | Open ContextId
    | FocusResult FocusResult
    | ElementResult ElementResult
    | BackdropClicked DomId


type alias Config msg =
    { toMsg : Msg -> msg
    , selected : Action -> ContextId -> msg
    }


getPopperDomId =
    .cid >> (++) "context-popup-"


getBackdropDomId =
    getPopperDomId >> (++) "-backdrop"


refIdFromCid cid =
    "context-popup-ref" ++ cid


getMaybeAutoFocusDomId model =
    actions |> List.head |> Maybe.map (getChildDomId (getPopperDomId model))


type alias ElementResult =
    Result Browser.Dom.Error Element


attemptGetElement : (ElementResult -> msg) -> DomId -> Cmd msg
attemptGetElement resultToMsg domId =
    Browser.Dom.getElement domId
        |> Task.attempt resultToMsg


type OutType
    = ActionOut Action
    | CloseOut


type alias OutMsg =
    { type_ : OutType, cid : ContextId }


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update message =
    let
        setOpenAndContextId cid model =
            { model | cid = cid, refEle = Nothing }
    in
    (case message of
        FocusResult r ->
            addCmd (Log.focusResult "ContextPopup.elm" r)
                >> withNoOutMsg

        ElementResult (Err _) ->
            addCmd (Log.warn "ContextPopup.elm" [ "Element Not Found" ])
                >> withNoOutMsg

        ElementResult (Ok element) ->
            mapModel (\model -> { model | refEle = Just element })
                >> addEffect (getMaybeAutoFocusDomId >> Focus.attemptMaybe FocusResult)
                >> withNoOutMsg

        BackdropClicked targetId ->
            withMaybeOutMsg
                (maybeWhen (eqs targetId << getBackdropDomId) (OutMsg CloseOut << .cid))

        ActionClicked action ->
            withOutMsg (.cid >> OutMsg (ActionOut action))

        Open cid ->
            mapModel (setOpenAndContextId cid)
                >> addCmd (attemptGetElement ElementResult (refIdFromCid cid))
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


view : Model -> Html Msg
view model =
    case model.refEle of
        Just element ->
            viewPopup element model

        _ ->
            noHtml



--    HtmlX.when .open viewPopup


viewPopup : Element -> Model -> Html Msg
viewPopup ref model =
    let
        popperDomId =
            getPopperDomId model

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
            [ id <| getBackdropDomId model, onClickTargetId BackdropClicked ]
    in
    --    viewModalContent
    UI.backdrop backdropAttrs [ viewModalContent ]
