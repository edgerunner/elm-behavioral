module TicTacToe exposing (Cell(..), GameEvent(..), Player(..), automatedO, initialState)

import Behavior exposing (..)


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
    | Win Player Cell Cell Cell
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
    [ waitFor (Play X c1) [ detectWin2 X ( c2, c3 ) (Win X c1) ]
    , waitFor (Play O c1) [ detectWin2 O ( c2, c3 ) (Win O c1) ]
    , waitFor (Play X c2) [ detectWin2 X ( c1, c3 ) (Win X c2) ]
    , waitFor (Play O c2) [ detectWin2 O ( c1, c3 ) (Win O c2) ]
    , waitFor (Play X c3) [ detectWin2 X ( c2, c1 ) (Win X c3) ]
    , waitFor (Play O c3) [ detectWin2 O ( c2, c1 ) (Win O c3) ]
    ]


detectWin2 : Player -> ( Cell, Cell ) -> (Cell -> Cell -> GameEvent) -> Thread GameEvent
detectWin2 player ( c1, c2 ) win _ =
    [ waitFor (Play player c1) [ detectWin3 player c2 (win c1) ]
    , waitFor (Play player c2) [ detectWin3 player c1 (win c2) ]
    , waitFor (Play (other player) c1) []
    , waitFor (Play (other player) c2) []
    ]


detectWin3 : Player -> Cell -> (Cell -> GameEvent) -> Thread GameEvent
detectWin3 player cell win _ =
    [ waitFor (Play player cell) [ andThen <| request (win cell) [] ]
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
    [ wait
        (\event ->
            case event of
                Win _ _ _ _ ->
                    Continue [ blockMoves ]

                Tie ->
                    Continue [ blockMoves ]

                _ ->
                    Pause
        )
    ]


initialState : List (Thread GameEvent)
initialState =
    takeTurn X
        :: tieCountdown 9
        :: blockMovesAfterGameOver
        :: cellThreads
        ++ winningTripletThreads



-- Autonomous O


oPlaysAnywhere : List (Thread GameEvent)
oPlaysAnywhere =
    let
        anywhere cell _ =
            [ request (Play O cell) []
            , waitFor (Play X cell) []
            ]
    in
    List.map anywhere allCells


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
    , waitFor (Play O c1) []
    , waitFor (Play O c2) []
    , waitFor (Play O c3) []
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
    [ request (Play O cell) []
    , waitFor (Play O cell) []
    ]


oGoesForWinningMoves : List (Thread GameEvent)
oGoesForWinningMoves =
    winningTriplets
        |> List.map tripletOffense1


tripletOffense1 : ( Cell, Cell, Cell ) -> Thread GameEvent
tripletOffense1 ( c1, c2, c3 ) _ =
    [ waitFor (Play O c1) [ tripletOffense2 ( c2, c3 ) ]
    , waitFor (Play O c2) [ tripletOffense2 ( c1, c3 ) ]
    , waitFor (Play O c3) [ tripletOffense2 ( c1, c2 ) ]
    , waitFor (Play X c1) []
    , waitFor (Play X c2) []
    , waitFor (Play X c3) []
    ]


tripletOffense2 : ( Cell, Cell ) -> Thread GameEvent
tripletOffense2 ( c1, c2 ) _ =
    [ waitFor (Play O c1) [ tripletOffense3 c2 ]
    , waitFor (Play O c2) [ tripletOffense3 c1 ]
    , waitFor (Play X c1) []
    , waitFor (Play X c2) []
    ]


tripletOffense3 : Cell -> Thread GameEvent
tripletOffense3 cell _ =
    [ request (Play O cell) [] ]


automatedO : List (Thread GameEvent)
automatedO =
    List.concat
        [ oGoesForWinningMoves
        , oDefendsTriplets
        , [ oPrefersCenter ]
        , oPlaysAnywhere
        ]
