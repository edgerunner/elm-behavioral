module TapControlTest exposing (suite)

import Behavior exposing (..)
import Expect
import TapControl exposing (..)
import Test exposing (..)


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
