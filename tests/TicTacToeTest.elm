module TicTacToeTest exposing (boardEvent, game, restarting, singlePlayerGame)

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


boardEvent : Test
boardEvent =
    describe "Game board"
        [ test "plays are marked" <|
            \_ ->
                initialize (initialState ++ board)
                    |> fire (Play X Top)
                    |> fire (Play O Center)
                    |> expectOneEvent
                        (Board
                            { topLeft = Blank
                            , top = Marked X
                            , topRight = Blank
                            , left = Blank
                            , center = Marked O
                            , right = Blank
                            , bottomLeft = Blank
                            , bottom = Blank
                            , bottomRight = Blank
                            }
                        )
        , test "wins are highlighted" <|
            \_ ->
                initialize (initialState ++ board)
                    |> fire (Play X Top)
                    |> fire (Play O Left)
                    |> fire (Play X Center)
                    |> fire (Play O Right)
                    |> fire (Play X Bottom)
                    |> expectOneEvent
                        (Board
                            { topLeft = Blank
                            , top = Highlighted X
                            , topRight = Blank
                            , left = Marked O
                            , center = Highlighted X
                            , right = Marked O
                            , bottomLeft = Blank
                            , bottom = Highlighted X
                            , bottomRight = Blank
                            }
                        )
        ]


restarting : Test
restarting =
    describe "Restarting"
        [ test "A finished game can be restarted" <|
            \_ ->
                (initialState ++ board)
                    |> restartable
                    |> initialize
                    |> fire (Play X Top)
                    |> fire (Play O TopLeft)
                    |> fire (Play X Center)
                    |> fire (Play O Left)
                    |> fire (Play X Bottom)
                    |> fire Restart
                    |> fire (Play X Right)
                    |> expectOneEvent
                        (Board
                            { topLeft = Blank
                            , top = Blank
                            , topRight = Blank
                            , left = Blank
                            , center = Blank
                            , right = Marked X
                            , bottomLeft = Blank
                            , bottom = Blank
                            , bottomRight = Blank
                            }
                        )
        ]
