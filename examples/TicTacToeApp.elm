module TicTacToeApp exposing (main)

import Behavior exposing (State)
import Behavior.Program
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import TicTacToe exposing (..)



-- PROGRAM


main : Program () (State GameEvent) GameEvent
main =
    Behavior.Program.sandbox
        { view = view
        , behavior = behavior
        }



-- MODEL ie. BEHAVIOR


behavior : State GameEvent
behavior =
    Behavior.initialize
        (board ++ automatedO ++ initialState)



-- UPDATE
-- VIEW


view : State GameEvent -> Html GameEvent
view state =
    let
        { topLeft, top, topRight, left, center, right, bottomLeft, bottom, bottomRight } =
            extractGrid state
    in
    div []
        [ Html.node "style" [] [ text css ]
        , div [] <| List.map2 cell [ TopLeft, Top, TopRight ] [ topLeft, top, topRight ]
        , div [] <| List.map2 cell [ Left, Center, Right ] [ left, center, right ]
        , div [] <| List.map2 cell [ BottomLeft, Bottom, BottomRight ] [ bottomLeft, bottom, bottomRight ]
        ]


extractGrid : State GameEvent -> Grid
extractGrid state =
    let
        latestBoardUpdate eventLog =
            case eventLog of
                [] ->
                    empty

                (Board grid) :: _ ->
                    grid

                _ :: rest ->
                    latestBoardUpdate rest
    in
    latestBoardUpdate <| Behavior.log state


cell : Cell -> Mark -> Html GameEvent
cell currentCell cellMark =
    let
        markChar =
            case cellMark of
                Blank ->
                    ""

                Marked X ->
                    "X"

                Marked O ->
                    "O"

                Highlighted X ->
                    "X"

                Highlighted O ->
                    "O"

        highlightClass =
            case cellMark of
                Blank ->
                    class "blank"

                Marked _ ->
                    class "plain"

                Highlighted _ ->
                    class "highlighted"
    in
    button [ onClick (Click currentCell), highlightClass ] [ text markChar ]


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
