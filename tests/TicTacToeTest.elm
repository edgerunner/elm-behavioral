module TicTacToeTest exposing (game, singlePlayerGame)

import Behavior exposing (..)
import Expect
import Test exposing (..)


type Cell
    = Center
    | Top
    | TopLeft
    | Left
    | BottomLeft
    | Bottom
    | BottomRight
    | Right
    | TopRight


type Player
    = X
    | O


other : Player -> Player
other player =
    case player of
        X ->
            O

        O ->
            X


type GameEvent
    = Click Cell
    | Play Player Cell
    | Win Player
    | Tie


playOnClick : Cell -> Thread GameEvent
playOnClick cell _ =
    let
        playAnyMark _ =
            [ request (Play X cell) []
            , request (Play O cell) []
            ]
    in
    [ waitFor (Click cell) [ playAnyMark ] ]


takeTurn : Player -> Thread GameEvent
takeTurn player _ =
    [ wait
        (\event ->
            case ( player, event ) of
                ( X, Play X _ ) ->
                    Continue [ takeTurn O ]

                ( O, Play O _ ) ->
                    Continue [ takeTurn X ]

                _ ->
                    Pause
        )
    , block
        (\event ->
            case ( player, event ) of
                ( X, Play O _ ) ->
                    Blocked

                ( O, Play X _ ) ->
                    Blocked

                _ ->
                    Free
        )
    ]


playCellOnlyOnce : Cell -> Thread GameEvent
playCellOnlyOnce cell _ =
    let
        blockCell _ =
            [ blockEvent (Play O cell)
            , blockEvent (Play X cell)
            ]
    in
    [ waitFor (Play X cell) [ blockCell ]
    , waitFor (Play O cell) [ blockCell ]
    ]


allCells : List Cell
allCells =
    [ TopLeft
    , Top
    , Center
    , Left
    , BottomLeft
    , Bottom
    , BottomRight
    , Right
    , TopRight
    ]


cellThreads : List (Thread GameEvent)
cellThreads =
    List.concatMap
        (\cell ->
            [ playOnClick cell, playCellOnlyOnce cell ]
        )
        allCells


detectWin1 : ( Cell, Cell, Cell ) -> Thread GameEvent
detectWin1 ( c1, c2, c3 ) _ =
    [ waitFor (Play X c1) [ detectWin2 X ( c2, c3 ) ]
    , waitFor (Play O c1) [ detectWin2 O ( c2, c3 ) ]
    , waitFor (Play X c2) [ detectWin2 X ( c1, c3 ) ]
    , waitFor (Play O c2) [ detectWin2 O ( c1, c3 ) ]
    , waitFor (Play X c3) [ detectWin2 X ( c2, c1 ) ]
    , waitFor (Play O c3) [ detectWin2 O ( c2, c1 ) ]
    ]


detectWin2 : Player -> ( Cell, Cell ) -> Thread GameEvent
detectWin2 player ( c1, c2 ) _ =
    [ waitFor (Play player c1) [ detectWin3 player c2 ]
    , waitFor (Play player c2) [ detectWin3 player c1 ]
    , waitFor (Play (other player) c1) []
    , waitFor (Play (other player) c2) []
    ]


detectWin3 : Player -> Cell -> Thread GameEvent
detectWin3 player cell _ =
    [ waitFor (Play player cell) [ andThen <| request (Win player) [] ]
    , waitFor (Play (other player) cell) []
    ]


winningTriplets : List ( Cell, Cell, Cell )
winningTriplets =
    [ ( TopLeft, Top, TopRight )
    , ( Left, Center, Right )
    , ( BottomLeft, Bottom, BottomRight )
    , ( TopLeft, Left, BottomLeft )
    , ( Top, Center, Bottom )
    , ( TopRight, Right, BottomRight )
    , ( TopLeft, Center, BottomRight )
    , ( TopRight, Center, BottomLeft )
    ]


winningTripletThreads : List (Thread GameEvent)
winningTripletThreads =
    List.map detectWin1 winningTriplets


tieCountdown : Int -> Thread GameEvent
tieCountdown count _ =
    case count of
        0 ->
            [ request Tie [] ]

        _ ->
            [ wait
                (\event ->
                    case event of
                        Play _ _ ->
                            Continue [ tieCountdown (count - 1) ]

                        _ ->
                            Pause
                )
            ]


blockMovesAfterGameOver : Thread GameEvent
blockMovesAfterGameOver _ =
    let
        blockMoves =
            andThen <|
                block
                    (\event ->
                        case event of
                            Play _ _ ->
                                Blocked

                            _ ->
                                Free
                    )
    in
    [ waitFor Tie [ blockMoves ]
    , waitFor (Win X) [ blockMoves ]
    , waitFor (Win O) [ blockMoves ]
    ]


initialState : List (Thread GameEvent)
initialState =
    takeTurn X
        :: tieCountdown 9
        :: blockMovesAfterGameOver
        :: cellThreads
        ++ winningTripletThreads



-- Autonomous O


oPlaysAnywhere : Thread GameEvent
oPlaysAnywhere _ =
    allCells
        |> List.map (request << Play O)
        |> List.map ((|>) [ oPlaysAnywhere ])


oPrefersCenter : Thread GameEvent
oPrefersCenter _ =
    [ request (Play O Center) []
    , waitFor (Play X Center) []
    , block
        (\event ->
            case event of
                Play O Center ->
                    Free

                Play O _ ->
                    Blocked

                _ ->
                    Free
        )
    ]


oDefendsTriplets : List (Thread GameEvent)
oDefendsTriplets =
    winningTriplets
        |> List.map tripletDefense1


tripletDefense1 : ( Cell, Cell, Cell ) -> Thread GameEvent
tripletDefense1 ( c1, c2, c3 ) _ =
    [ waitFor (Play X c1) [ tripletDefense2 ( c2, c3 ) ]
    , waitFor (Play X c2) [ tripletDefense2 ( c1, c3 ) ]
    , waitFor (Play X c3) [ tripletDefense2 ( c1, c2 ) ]
    ]


tripletDefense2 : ( Cell, Cell ) -> Thread GameEvent
tripletDefense2 ( c1, c2 ) _ =
    [ waitFor (Play X c1) [ tripletDefense3 c2 ]
    , waitFor (Play X c2) [ tripletDefense3 c1 ]
    , waitFor (Play O c1) []
    , waitFor (Play O c2) []
    ]


tripletDefense3 : Cell -> Thread GameEvent
tripletDefense3 cell _ =
    let
        allOthers event =
            case event of
                Play O c ->
                    if c == cell then
                        Free

                    else
                        Blocked

                _ ->
                    Free
    in
    [ request (Play O cell) []
    , block allOthers
    ]


automatedO : List (Thread GameEvent)
automatedO =
    oPlaysAnywhere :: oPrefersCenter :: oDefendsTriplets



-- Test utilities


expectLastEvent : GameEvent -> State GameEvent -> Expect.Expectation
expectLastEvent event state =
    state
        |> log
        |> List.head
        |> Expect.equal (Just event)



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
                    |> expectLastEvent (Win X)
        , test "O can play a diagonal and win" <|
            \_ ->
                initialize initialState
                    |> fire (Click Top)
                    |> fire (Click Center)
                    |> fire (Click Left)
                    |> fire (Click TopLeft)
                    |> fire (Click Bottom)
                    |> fire (Click BottomRight)
                    |> expectLastEvent (Win O)
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
                    |> expectLastEvent Tie
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
                    |> expectLastEvent (Play O Center)
        , test "prevents X from completing a triplet" <|
            \_ ->
                initialize (automatedO ++ initialState)
                    |> fire (Play X Center)
                    |> fireOne [ Play X Left, Play X Right ]
                    |> fireOne [ Play X Left, Play X Right ]
                    |> log
                    |> List.head
                    |> Expect.notEqual
                        (Just <| Win X)
        ]
