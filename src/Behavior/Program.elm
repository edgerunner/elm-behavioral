module Behavior.Program exposing (dualSandbox, sandbox)

import Behavior exposing (State, fire)
import Browser
import Html exposing (Html)


sandbox :
    { init : State event
    , view : State event -> Html event
    }
    -> Program () (State event) event
sandbox record =
    Browser.sandbox
        { init = record.init
        , view = record.view
        , update = fire
        }


dualSandbox :
    { init : ( State event, model )
    , view : ( State event, model ) -> Html event
    , update : State event -> model -> model
    }
    -> Program () ( State event, model ) event
dualSandbox record =
    Browser.sandbox
        { init = record.init
        , view = record.view
        , update = updateDualSandbox record.update
        }


updateDualSandbox :
    (State event -> model -> model)
    -> event
    -> ( State event, model )
    -> ( State event, model )
updateDualSandbox update event ( oldState, model ) =
    let
        newState =
            fire event oldState
    in
    ( newState, update newState model )
