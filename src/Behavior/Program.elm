module Behavior.Program exposing (sandbox)

import Behavior exposing (State)
import Browser
import Html exposing (Html)


sandbox :
    { init : model
    , behavior : State event
    , reduce : event -> model -> model
    , view : model -> Html event
    }
    -> Program () ( State event, model ) event
sandbox record =
    Browser.sandbox
        { init = ( record.behavior, record.init )
        , view = record.view << Tuple.second
        , update = updateSandbox record.reduce
        }


updateSandbox :
    (event -> model -> model)
    -> event
    -> ( State event, model )
    -> ( State event, model )
updateSandbox reduce event ( state, model ) =
    let
        newState =
            Behavior.fire event state

        recentEvents =
            Behavior.recent newState

        updatedModel =
            List.foldr reduce model recentEvents
    in
    ( newState, updatedModel )
