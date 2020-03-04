module Behavior.Program exposing (sandbox)

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
