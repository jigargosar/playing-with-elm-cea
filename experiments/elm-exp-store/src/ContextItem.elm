module ContextItem exposing (ContextItem, allContextItems, viewSelectContext)

import ContextStore exposing (ContextId, ContextStore)
import Html exposing (Html)
import SelectUI


type alias ContextItem =
    ( String, ContextId )


allContextItems : ContextStore -> List ContextItem
allContextItems =
    ContextStore.list
        >> List.map (\c -> ( c.name, c.id ))
        >> (::) ( ContextStore.defaultName, ContextStore.defaultId )


contextItemWithId : ContextStore -> ContextId -> ContextItem
contextItemWithId contextStore contextId =
    ( ContextStore.getNameOrDefaultById contextId contextStore, contextId )


viewSelectContext : SelectUI.Config msg ContextItem -> ContextStore -> ContextId -> SelectUI.Model -> Html (SelectUI.Msg ContextItem)
viewSelectContext config contextStore currentContextId selectUIModel =
    SelectUI.view config
        (Just <| contextItemWithId contextStore currentContextId)
        (allContextItems contextStore)
        selectUIModel
