module ContextPopup exposing
    ( Action(..)
    , Model
    , Msg
    , init
    , isOpenForContextId
    , refIdFromCid
    , subscriptions
    , toggleOpenFor
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
    { open : Bool
    , cid : ContextId
    , refEle : Maybe Element
    }


init : ContextId -> Model
init cid =
    { open = False
    , cid = cid
    , refEle = Nothing
    }


isOpenForContextId cid model =
    model.cid == cid && model.open


type alias BounceMsg =
    Maybe Msg


toggleOpenFor =
    ToggleOpenFor


type Msg
    = ActionClicked Action
    | ToggleOpenFor ContextId
    | FocusResult FocusResult
    | ElementResult ElementResult
    | BackdropClicked DomId


subscriptions model =
    Sub.batch
        []


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


setClosed =
    \model -> { model | open = False, refEle = Nothing }


setOpenAndContextId cid model =
    { model | open = True, cid = cid, refEle = Nothing }


type alias ElementResult =
    Result Browser.Dom.Error Element


attemptGetElement : (ElementResult -> msg) -> DomId -> Cmd msg
attemptGetElement resultToMsg domId =
    Browser.Dom.getElement domId
        |> Task.attempt resultToMsg


update : (Action -> ContextId -> msg) -> Msg -> Model -> ( Model, Cmd Msg, Maybe msg )
update onAction message =
    (case message of
        FocusResult r ->
            addCmd (Log.focusResult "ContextPopup.elm" r)
                >> withNoOutMsg

        ElementResult (Err _) ->
            addCmd (Log.warn "ContextPopup.elm" [ "Element Not Found" ])
                >> withNoOutMsg

        ElementResult (Ok element) ->
            let
                _ =
                    Debug.log "element" element
            in
            mapModel (\model -> { model | refEle = Just element })
                >> addEffect (getMaybeAutoFocusDomId >> Focus.attemptMaybe FocusResult)
                >> withNoOutMsg

        BackdropClicked targetId ->
            mapWhen (getBackdropDomId >> eqs targetId)
                (mapModel setClosed)
                >> withNoOutMsg

        ActionClicked child ->
            mapModel setClosed
                >> withOutMsg (.cid >> onAction child)

        ToggleOpenFor cid ->
            mapIfElse (isOpenForContextId cid)
                (mapModel setClosed)
                (mapModel (setOpenAndContextId cid)
                    >> addCmd (attemptGetElement ElementResult (refIdFromCid cid))
                )
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
    case ( model.open, model.refEle ) of
        ( True, Just element ) ->
            viewPopup element model

        _ ->
            noHtml



--    HtmlX.when .open viewPopup


viewPopup : Element -> Model -> Html Msg
viewPopup element model =
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
            , left (px <| element.element.x + element.element.width)
            , top (px <| min element.element.y (element.viewport.height - 100))
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
