module TicTacToe exposing
    ( Cell(..)
    , GameEvent(..)
    , Grid
    , Mark(..)
    , Player(..)
    , automatedO
    , board
    , empty
    , endgame
    , initialState
    , restartable
    , singlePlayer
    , turn
    , twoPlayers
    )

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
    | Board Grid
    | Restart


type alias Grid =
    { topLeft : Mark
    , top : Mark
    , topRight : Mark
    , left : Mark
    , center : Mark
    , right : Mark
    , bottomLeft : Mark
    , bottom : Mark
    , bottomRight : Mark
    }


type Mark
    = Blank
    | Marked Player
    | Highlighted Player


playOnClick : Cell -> Thread GameEvent
playOnClick cell _ =
    let
        playAnyMark _ =
            [ request (Play X cell) []
            , request (Play O cell) []
            ]
    in
    [ waitFor (Click cell) [ playAnyMark ]
    , waitFor Restart []
    ]


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
    , waitFor Restart []
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
        blockCell () =
            [ blockEvent (Play O cell)
            , blockEvent (Play X cell)
            , waitFor Restart []
            ]
    in
    [ waitFor (Play X cell) [ blockCell ]
    , waitFor (Play O cell) [ blockCell ]
    , waitFor Restart []
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
    , waitFor Restart []
    ]


detectWin2 : Player -> ( Cell, Cell ) -> (Cell -> Cell -> GameEvent) -> Thread GameEvent
detectWin2 player ( c1, c2 ) win _ =
    [ waitFor (Play player c1) [ detectWin3 player c2 (win c1) ]
    , waitFor (Play player c2) [ detectWin3 player c1 (win c2) ]
    , waitFor (Play (other player) c1) []
    , waitFor (Play (other player) c2) []
    , waitFor Restart []
    ]


detectWin3 : Player -> Cell -> (Cell -> GameEvent) -> Thread GameEvent
detectWin3 player cell win _ =
    [ waitFor (Play player cell) [ andThen <| request (win cell) [], blockPlayUntilRestart ]
    , waitFor (Play (other player) cell) []
    , waitFor Restart []
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
            [ request Tie [ blockPlayUntilRestart ] ]

        _ ->
            [ wait
                (\event ->
                    case event of
                        Play _ _ ->
                            Continue [ tieCountdown (count - 1) ]

                        _ ->
                            Pause
                )
            , waitFor Restart []
            ]


blockPlayUntilRestart : Thread GameEvent
blockPlayUntilRestart _ =
    [ waitFor Restart []
    , blockPlay
    ]


blockPlay : Behavior GameEvent
blockPlay =
    block
        (\event ->
            case event of
                Play _ _ ->
                    Blocked

                _ ->
                    Free
        )


initialState : List (Thread GameEvent)
initialState =
    takeTurn X
        :: tieCountdown 9
        :: winningTripletThreads
        ++ cellThreads



-- Autonomous O


oPlaysAnywhere : List (Thread GameEvent)
oPlaysAnywhere =
    let
        anywhere cell _ =
            [ request (Play O cell) []
            , waitFor (Play X cell) []
            , waitFor Restart []
            ]
    in
    List.map anywhere allCells


oPrefersCenter : Thread GameEvent
oPrefersCenter _ =
    [ request (Play O Center) []
    , waitFor (Play X Center) []
    , waitFor Restart []
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
    , waitFor Restart []
    ]


tripletDefense2 : ( Cell, Cell ) -> Thread GameEvent
tripletDefense2 ( c1, c2 ) _ =
    [ waitFor (Play X c1) [ tripletDefense3 c2 ]
    , waitFor (Play X c2) [ tripletDefense3 c1 ]
    , waitFor (Play O c1) []
    , waitFor (Play O c2) []
    , waitFor Restart []
    ]


tripletDefense3 : Cell -> Thread GameEvent
tripletDefense3 cell _ =
    [ request (Play O cell) []
    , waitFor (Play O cell) []
    , waitFor Restart []
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
    , waitFor Restart []
    ]


tripletOffense2 : ( Cell, Cell ) -> Thread GameEvent
tripletOffense2 ( c1, c2 ) _ =
    [ waitFor (Play O c1) [ tripletOffense3 c2 ]
    , waitFor (Play O c2) [ tripletOffense3 c1 ]
    , waitFor (Play X c1) []
    , waitFor (Play X c2) []
    , waitFor Restart []
    ]


tripletOffense3 : Cell -> Thread GameEvent
tripletOffense3 cell _ =
    [ request (Play O cell) []
    , waitFor Restart []
    ]


automatedO : List (Thread GameEvent)
automatedO =
    List.concat
        [ oGoesForWinningMoves
        , oDefendsTriplets
        , [ oPrefersCenter ]
        , oPlaysAnywhere
        ]



-- GRID STATE


empty : Grid
empty =
    { topLeft = Blank
    , top = Blank
    , topRight = Blank
    , left = Blank
    , center = Blank
    , right = Blank
    , bottomLeft = Blank
    , bottom = Blank
    , bottomRight = Blank
    }


updateBoard : Grid -> Thread GameEvent
updateBoard grid _ =
    [ wait
        (\event ->
            case event of
                Play player cell ->
                    Continue <| List.map ((|>) (markGrid player cell grid)) [ updateBoard, publishBoard ]

                Win player c1 c2 c3 ->
                    Continue
                        [ publishBoard <|
                            List.foldl (highlightGrid player) grid [ c1, c2, c3 ]
                        ]

                Restart ->
                    Continue []

                _ ->
                    Pause
        )
    ]


publishBoard : Grid -> Thread GameEvent
publishBoard grid _ =
    [ request (Board grid) [ updateBoard grid ]
    , waitFor Restart []
    ]


markGrid : Player -> Cell -> Grid -> Grid
markGrid player =
    updateGridCell <| Marked player


highlightGrid : Player -> Cell -> Grid -> Grid
highlightGrid player =
    updateGridCell <| Highlighted player


updateGridCell : Mark -> Cell -> Grid -> Grid
updateGridCell mark cell grid =
    case cell of
        TopLeft ->
            { grid | topLeft = mark }

        Top ->
            { grid | top = mark }

        TopRight ->
            { grid | topRight = mark }

        Left ->
            { grid | left = mark }

        Center ->
            { grid | center = mark }

        Right ->
            { grid | right = mark }

        BottomLeft ->
            { grid | bottomLeft = mark }

        Bottom ->
            { grid | bottom = mark }

        BottomRight ->
            { grid | bottomRight = mark }


board : List (Thread GameEvent)
board =
    [ updateBoard empty, publishBoard empty ]



-- RESTARTABLE GAME


restartable : List (Thread GameEvent) -> List (Thread GameEvent)
restartable startingBehaviors =
    resetGameOnRestart startingBehaviors :: startingBehaviors


resetGameOnRestart : List (Thread GameEvent) -> Thread GameEvent
resetGameOnRestart startingBehaviors _ =
    [ waitFor Restart <| restartable startingBehaviors ]



-- HELPERS


turn : State GameEvent -> Maybe Player
turn =
    let
        findTurn events =
            case events of
                [] ->
                    Just X

                (Play X _) :: _ ->
                    Just O

                (Play O _) :: _ ->
                    Just X

                Restart :: _ ->
                    Just X

                (Win _ _ _ _) :: _ ->
                    Nothing

                Tie :: _ ->
                    Nothing

                _ :: more ->
                    findTurn more
    in
    log >> findTurn


endgame : State GameEvent -> Maybe GameEvent
endgame =
    let
        findEndgame events =
            case events of
                [] ->
                    Nothing

                Restart :: _ ->
                    Nothing

                Tie :: _ ->
                    Just Tie

                (Win player c1 c2 c3) :: _ ->
                    Just <| Win player c1 c2 c3

                _ :: more ->
                    findEndgame more
    in
    log >> findEndgame



-- INITIALIZATION


singlePlayer : State GameEvent
singlePlayer =
    Behavior.initialize <|
        restartable (board ++ automatedO ++ initialState)


twoPlayers : State GameEvent
twoPlayers =
    Behavior.initialize <|
        restartable (board ++ initialState)
