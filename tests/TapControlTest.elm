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
        [ Request event [ add event (duration - 1) ] ]

    else
        []


mix : Thread TapEvent
mix _ =
    [ block Hot
    , wait Cold
        [ \_ ->
            [ block Cold
            , wait Hot [ mix ]
            ]
        ]
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
                initialize [ add Cold 3, add Hot 3, mix ]
                    |> log
                    |> Expect.equalLists
                        [ Hot, Cold, Hot, Cold, Hot, Cold ]
        ]
