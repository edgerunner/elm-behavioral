module TapControl exposing (TapEvent(..), add, mix)

import Behavior exposing (..)


type TapEvent
    = Hot
    | Cold


add : TapEvent -> Int -> Thread TapEvent
add event duration _ =
    if duration > 0 then
        [ request event [ add event (duration - 1) ] ]

    else
        []


mix : TapEvent -> TapEvent -> Thread TapEvent
mix tap1 tap2 _ =
    [ blockEvent tap1
    , waitFor tap2 [ mix tap2 tap1 ]
    ]
