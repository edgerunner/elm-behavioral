module TicTacToeApp exposing (main)

import Behavior exposing (State)
import Behavior.Program
import Css
import Css.Global exposing (Snippet)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Styled
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
        [ Css.Global.global [ style ] |> Html.Styled.toUnstyled
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


style : Snippet
style =
    Css.Global.body
        [ Css.Global.children
            [ Css.Global.div
                [ Css.property "display" "grid"
                , Css.property "grid-template-columns" "repeat(3,6rem)"
                , Css.property "grid-template-rows" "repeat(3,6rem)"
                , Css.padding (Css.em 0.5)
                , Css.property "gap" "0.5em"
                , Css.justifyContent Css.center
                , Css.property "background-color" "coral"
                , Css.Global.children
                    [ Css.Global.div
                        [ Css.property "display" "contents" ]
                    ]
                , Css.Global.descendants
                    [ Css.Global.button
                        [ Css.fontSize (Css.rem 3)
                        , Css.backgroundColor (Css.rgb 255 255 255)
                        , Css.borderStyle Css.none
                        , Css.Global.withClass "highlighted"
                            [ Css.backgroundColor (Css.rgb 0 0 0)
                            , Css.color (Css.rgb 255 255 255)
                            ]
                        ]
                    ]
                ]
            ]
        ]
