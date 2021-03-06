module TicTacToeTest exposing (game, singlePlayerGame)

import Behavior exposing (..)
import Expect
import Test exposing (..)
import TicTacToe exposing (..)



-- Test utilities


expectLastEvent : GameEvent -> State GameEvent -> Expect.Expectation
expectLastEvent event state =
    state
        |> log
        |> List.head
        |> Expect.equal (Just event)


expectOneEvent : GameEvent -> State GameEvent -> Expect.Expectation
expectOneEvent event state =
    let
        check eventLog =
            case eventLog of
                [] ->
                    Expect.fail "Event not found"

                first :: rest ->
                    if first == event then
                        Expect.pass

                    else
                        check rest
    in
    check (log state)



-- Actual testing


game : Test
game =
    describe "TicTacToe"
        [ test "clicks are processed in turns" <|
            \_ ->
                initialize initialState
                    |> fire (Click Center)
                    |> fire (Click Top)
                    |> fire (Click TopLeft)
                    |> log
                    |> Expect.equalLists
                        [ Play X TopLeft
                        , Click TopLeft
                        , Play O Top
                        , Click Top
                        , Play X Center
                        , Click Center
                        ]
        , test "clicks on the same spot do not yield moves" <|
            \_ ->
                initialize initialState
                    |> fire (Click Center)
                    |> fire (Click Center)
                    |> fire (Click Center)
                    |> log
                    |> Expect.equalLists
                        [ Click Center
                        , Click Center
                        , Play X Center
                        , Click Center
                        ]
        , test "playing a column results in a win" <|
            \_ ->
                initialize initialState
                    |> fire (Click Center)
                    |> fire (Click Left)
                    |> fire (Click Top)
                    |> fire (Click Right)
                    |> fire (Click Bottom)
                    |> expectOneEvent (Win X Center Top Bottom)
        , test "O can play a diagonal and win" <|
            \_ ->
                initialize initialState
                    |> fire (Click Top)
                    |> fire (Click Center)
                    |> fire (Click Left)
                    |> fire (Click TopLeft)
                    |> fire (Click Bottom)
                    |> fire (Click BottomRight)
                    |> expectOneEvent (Win O Center TopLeft BottomRight)
        , test "running out of space before a win is a tie" <|
            \_ ->
                initialize initialState
                    |> fire (Click Center)
                    |> fire (Click TopLeft)
                    |> fire (Click BottomLeft)
                    |> fire (Click TopRight)
                    |> fire (Click Top)
                    |> fire (Click Bottom)
                    |> fire (Click Left)
                    |> fire (Click Right)
                    |> fire (Click BottomRight)
                    |> expectOneEvent Tie
        , test "playing is not possible after a win. " <|
            \_ ->
                initialize initialState
                    |> fire (Click Center)
                    |> fire (Click Left)
                    |> fire (Click Top)
                    |> fire (Click Right)
                    |> fire (Click Bottom)
                    |> fire (Click BottomLeft)
                    |> expectLastEvent (Click BottomLeft)
        ]


singlePlayerGame : Test
singlePlayerGame =
    describe "Single player game with autonomous O"
        [ test "plays in any available cell" <|
            \_ ->
                initialize (automatedO ++ initialState)
                    |> fire (Click TopRight)
                    |> fire (Click Left)
                    |> log
                    |> List.filter
                        (\event ->
                            case event of
                                Play O _ ->
                                    True

                                _ ->
                                    False
                        )
                    |> List.length
                    |> Expect.equal 2
        , test "starts in the center if available" <|
            \_ ->
                initialize (automatedO ++ initialState)
                    |> fire (Click TopRight)
                    |> expectOneEvent (Play O Center)
        , test "prevents X from completing a triplet" <|
            \_ ->
                initialize (automatedO ++ initialState)
                    |> fire (Play X Center)
                    |> fireOne [ Play X Left, Play X Right ]
                    |> fireOne [ Play X Left, Play X Right ]
                    |> log
                    |> List.head
                    |> Expect.all
                        [ Expect.notEqual (Just <| Win X Center Left Right)
                        , Expect.notEqual (Just <| Win X Center Right Left)
                        ]
        ]
