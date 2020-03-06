module TicTacToeApp exposing (main)

import Behavior
import Behavior.Program
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import TicTacToe exposing (..)



-- PROGRAM


main : Program () StateAndGrid GameEvent
main =
    Behavior.Program.sandbox
        { init = init
        , view = view
        , behavior = behavior
        , reduce = reduce
        }



-- MODEL


type alias StateAndGrid =
    ( Behavior.State GameEvent, GridMarks )


type CellMark
    = Blank
    | Marked Player Highlight


type Highlight
    = Plain
    | Highlighted


type alias RowMarks =
    ( CellMark, CellMark, CellMark )


type alias GridMarks =
    ( RowMarks, RowMarks, RowMarks )


blankGrid : GridMarks
blankGrid =
    ( ( Blank, Blank, Blank )
    , ( Blank, Blank, Blank )
    , ( Blank, Blank, Blank )
    )


behavior : Behavior.State GameEvent
behavior =
    Behavior.initialize
        (automatedO ++ initialState)


init : GridMarks
init =
    blankGrid



-- UPDATE


reduce : GameEvent -> GridMarks -> GridMarks
reduce event grid =
    case event of
        Play player cell_ ->
            updateGrid (always <| Marked player Plain) cell_ grid

        Win player c1 c2 c3 ->
            grid
                |> updateGrid (always <| Marked player Highlighted) c1
                |> updateGrid (always <| Marked player Highlighted) c2
                |> updateGrid (always <| Marked player Highlighted) c3

        _ ->
            grid



-- VIEW


view : GridMarks -> Html GameEvent
view grid =
    let
        ( ( topLeft, top, topRight ), ( left, center, right ), ( bottomLeft, bottom, bottomRight ) ) =
            grid
    in
    div []
        [ Html.node "style" [] [ text css ]
        , div [] <| List.map2 cell [ TopLeft, Top, TopRight ] [ topLeft, top, topRight ]
        , div [] <| List.map2 cell [ Left, Center, Right ] [ left, center, right ]
        , div [] <| List.map2 cell [ BottomLeft, Bottom, BottomRight ] [ bottomLeft, bottom, bottomRight ]
        ]


cell : Cell -> CellMark -> Html GameEvent
cell currentCell cellMark =
    let
        markChar =
            case cellMark of
                Blank ->
                    "-"

                Marked X _ ->
                    "X"

                Marked O _ ->
                    "O"

        highlightClass =
            case cellMark of
                Blank ->
                    class "blank"

                Marked _ Plain ->
                    class "plain"

                Marked _ Highlighted ->
                    class "highlighted"
    in
    button [ onClick (Click currentCell), highlightClass ] [ text markChar ]


updateGrid : (CellMark -> CellMark) -> Cell -> GridMarks -> GridMarks
updateGrid updater cell_ grid =
    case cell_ of
        TopLeft ->
            updateFirst (\row -> updateFirst updater row) grid

        Top ->
            updateFirst (\row -> updateSecond updater row) grid

        TopRight ->
            updateFirst (\row -> updateThird updater row) grid

        Left ->
            updateSecond (\row -> updateFirst updater row) grid

        Center ->
            updateSecond (\row -> updateSecond updater row) grid

        Right ->
            updateSecond (\row -> updateThird updater row) grid

        BottomLeft ->
            updateThird (\row -> updateFirst updater row) grid

        Bottom ->
            updateThird (\row -> updateSecond updater row) grid

        BottomRight ->
            updateThird (\row -> updateThird updater row) grid


updateFirst : (a -> a) -> ( a, a, a ) -> ( a, a, a )
updateFirst updater ( a, b, c ) =
    ( updater a, b, c )


updateSecond : (a -> a) -> ( a, a, a ) -> ( a, a, a )
updateSecond updater ( a, b, c ) =
    ( a, updater b, c )


updateThird : (a -> a) -> ( a, a, a ) -> ( a, a, a )
updateThird updater ( a, b, c ) =
    ( a, b, updater c )


css : String
css =
    """
body > div > div {
    display: contents;
}
body > div {
    display: grid;
    grid-template-columns: repeat(3,6rem);
    grid-template-rows: repeat(3,6rem);
    padding: .5em;
    gap: .5em;
    background-color: coral;
    justify-content: center;
}
button {
    font-size: 3rem;
    background: white;
    border: none;
}
button.highlighted {
    background: black;
    color: white;
}
"""
