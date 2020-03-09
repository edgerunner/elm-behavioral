module Behavior.Program exposing (sandbox)

import Behavior exposing (State)
import Browser
import Html exposing (Html)


sandbox :
    { behavior : State event
    , view : State event -> Html event
    }
    -> Program () (State event) event
sandbox record =
    Browser.sandbox
        { init = record.behavior
        , view = record.view
        , update = Behavior.fire
        }
