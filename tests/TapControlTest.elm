module TapControlTest exposing (..)

import Behavior exposing (..)
import Expect
import Test exposing (..)


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


suite : Test
suite =
    describe "Tap control"
        [ test "adding just cold water" <|
            \_ ->
                initialize [ add Cold 5 ]
                    |> log
                    |> Expect.equalLists
                        [ Cold, Cold, Cold, Cold, Cold ]
        , test "adding cold and hot water without a mixer" <|
            \_ ->
                initialize [ add Cold 3, add Hot 3 ]
                    |> log
                    |> Expect.equalLists
                        [ Hot, Hot, Hot, Cold, Cold, Cold ]
        , test "adding cold and hot water with a mixer" <|
            \_ ->
                initialize [ add Cold 3, add Hot 3, mix Hot Cold ]
                    |> log
                    |> Expect.equalLists
                        [ Hot, Cold, Hot, Cold, Hot, Cold ]
        ]
