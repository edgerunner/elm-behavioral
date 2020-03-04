module TicTacToeApp exposing (main)

import Behavior
import Behavior.Program
import Html exposing (Html, button, div, li, ol, text)
import Html.Attributes exposing (reversed)
import Html.Events exposing (onClick)
import TicTacToe exposing (..)



-- PROGRAM


main : Program () State GameEvent
main =
    Behavior.Program.sandbox
        { init = init
        , view = view
        }



-- MODEL


type alias State =
    Behavior.State GameEvent


init : State
init =
    Behavior.initialize
        (automatedO ++ initialState)



-- VIEW


view : State -> Html GameEvent
view state =
    div []
        [ div [] <| List.map (cell state) [ TopLeft, Top, TopRight ]
        , div [] <| List.map (cell state) [ Left, Center, Right ]
        , div [] <| List.map (cell state) [ BottomLeft, Bottom, BottomRight ]
        , ol [] <| List.map eventLi (Behavior.pending state)
        , ol [ reversed True ] <| List.map eventLi (Behavior.log state)
        ]


cell : State -> Cell -> Html GameEvent
cell state currentCell =
    let
        mark =
            List.foldl playMark "-" (Behavior.log state)

        playMark event str =
            case event of
                Play player eventCell ->
                    case ( eventCell == currentCell, player ) of
                        ( True, X ) ->
                            "X"

                        ( True, O ) ->
                            "O"

                        _ ->
                            str

                _ ->
                    str
    in
    button [ onClick (Click currentCell) ] [ text mark ]


eventLi : GameEvent -> Html GameEvent
eventLi event =
    li [] [ text <| Debug.toString event ]
