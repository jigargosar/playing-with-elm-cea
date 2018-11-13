module ContextPopup exposing
    ( Action(..)
    , Model
    , Msg
    , OutMsg(..)
    , getRefId
    , init
    , isOpenForContextId
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
    , popperEle : Maybe Element
    , context : Context
    }


init : Context -> Model
init context =
    { refEle = Nothing
    , popperEle = Nothing
    , context = context
    }


type alias BounceMsg =
    Maybe Msg


open =
    Open


type Msg
    = ActionClicked Action
    | Open
    | FocusResult FocusResult
    | RefElementResult ElementResult
    | PopperElementResult ElementResult
    | BackdropClicked DomId


type alias Config msg =
    { toMsg : Msg -> msg
    , selected : Action -> ContextId -> msg
    }


isOpenForContextId cid =
    getContextId >> eqs cid


getContextId =
    .context >> .id


getPopperDomId model =
    "context-popup-" ++ getContextId model


getBackdropDomId =
    getPopperDomId >> (++) "-backdrop"


getRefId model =
    "context-popup-ref" ++ getContextId model


getAutoFocusDomId model =
    getChildDomId (getPopperDomId model) 0


type alias ElementResult =
    Result Browser.Dom.Error Element


attemptGetElement : (ElementResult -> msg) -> DomId -> Cmd msg
attemptGetElement resultToMsg domId =
    Browser.Dom.getElement domId
        |> Task.attempt resultToMsg


type OutMsg
    = ActionOut Context Action
    | ClosedOut


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update message =
    (case message of
        FocusResult r ->
            addCmd (Log.focusResult "ContextPopup.elm" r)
                >> withNoOutMsg

        RefElementResult (Err _) ->
            addCmd (Log.warn "ContextPopup.elm" [ "Ref Element Not Found" ])
                >> withNoOutMsg

        RefElementResult (Ok element) ->
            mapModel (\model -> { model | refEle = Just element })
                >> addEffect (getAutoFocusDomId >> Focus.attempt FocusResult)
                >> addEffect (attemptGetElement PopperElementResult << getPopperDomId)
                >> withNoOutMsg

        PopperElementResult (Err _) ->
            addCmd (Log.warn "ContextPopup.elm" [ "Popper Element Not Found" ])
                >> withNoOutMsg

        PopperElementResult (Ok element) ->
            mapModel (\model -> { model | popperEle = Just element })
                >> addEffect (getAutoFocusDomId >> Focus.attempt FocusResult)
                >> withNoOutMsg

        BackdropClicked targetId ->
            withMaybeOutMsg
                (\model ->
                    if targetId == getBackdropDomId model then
                        Just ClosedOut

                    else
                        Nothing
                )

        ActionClicked action ->
            withOutMsg (\{ context } -> ActionOut context action)

        Open ->
            mapModel (\model -> { model | refEle = Nothing, popperEle = Nothing })
                >> addEffect (attemptGetElement RefElementResult << getRefId)
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


view : Model -> Html Msg
view model =
    case model.refEle of
        Just element ->
            viewPopup element model

        _ ->
            noHtml


viewPopup : Element -> Model -> Html Msg
viewPopup ref model =
    let
        context =
            model.context

        popperDomId =
            getPopperDomId model

        viewChild idx child =
            div
                [ onClick <| ActionClicked child ]
                (childContent idx context popperDomId child)

        rootLeft =
            model.popperEle
                |> unwrapMaybe (ref.element.x + ref.element.width)
                    (\pe ->
                        min (ref.element.x + ref.element.width) (ref.viewport.width - pe.element.width)
                    )

        rootTop =
            model.popperEle
                |> unwrapMaybe (ref.element.y + ref.element.height)
                    (\pe ->
                        min (ref.element.y + ref.element.height) (ref.viewport.height - pe.element.height)
                    )

        rootStyles =
            [ bg "white"
            , elevation 4
            , borderRadius (rem 0.5)
            , pRm 0.5
            , minWidth (rem 10)
            , position absolute
            , left (px <| rootLeft)
            , top (px <| rootTop)
            ]

        rootAttributes =
            [ id popperDomId
            ]

        viewModalContent =
            sDiv rootStyles
                rootAttributes
                (List.indexedMap viewChild actions)

        backdropAttrs =
            [ id <| getBackdropDomId model, onClickTargetId BackdropClicked ]
    in
    --    viewModalContent
    UI.backdrop backdropAttrs [ viewModalContent ]
