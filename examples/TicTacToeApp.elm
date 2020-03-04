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
    let
        ( ( topLeft, top, topRight ), ( left, center, right ), ( bottomLeft, bottom, bottomRight ) ) =
            marks state
    in
    div []
        [ Html.node "style" [] [ text css ]
        , div [] <| List.map2 cell [ TopLeft, Top, TopRight ] [ topLeft, top, topRight ]
        , div [] <| List.map2 cell [ Left, Center, Right ] [ left, center, right ]
        , div [] <| List.map2 cell [ BottomLeft, Bottom, BottomRight ] [ bottomLeft, bottom, bottomRight ]
        , ol [] <| List.map eventLi (Behavior.pending state)
        , ol [ reversed True ] <| List.map eventLi (Behavior.log state)
        ]


cell : Cell -> Maybe Player -> Html GameEvent
cell currentCell player =
    let
        mark =
            case player of
                Nothing ->
                    "-"

                Just X ->
                    "X"

                Just O ->
                    "O"
    in
    button [ onClick (Click currentCell) ] [ text mark ]


eventLi : GameEvent -> Html GameEvent
eventLi event =
    li [] [ text <| Debug.toString event ]


type alias RowMarks =
    ( Maybe Player, Maybe Player, Maybe Player )


type alias GridMarks =
    ( RowMarks, RowMarks, RowMarks )


marks : State -> GridMarks
marks state =
    let
        blankGrid : GridMarks
        blankGrid =
            ( ( Nothing, Nothing, Nothing )
            , ( Nothing, Nothing, Nothing )
            , ( Nothing, Nothing, Nothing )
            )

        mark event (( top, mid, bottom ) as grid) =
            case event of
                Play player cell_ ->
                    case cell_ of
                        TopLeft ->
                            updateFirst (updateFirst (Just player) top) grid

                        Top ->
                            updateFirst (updateSecond (Just player) top) grid

                        TopRight ->
                            updateFirst (updateThird (Just player) top) grid

                        Left ->
                            updateSecond (updateFirst (Just player) mid) grid

                        Center ->
                            updateSecond (updateSecond (Just player) mid) grid

                        Right ->
                            updateSecond (updateThird (Just player) mid) grid

                        BottomLeft ->
                            updateThird (updateFirst (Just player) bottom) grid

                        Bottom ->
                            updateThird (updateSecond (Just player) bottom) grid

                        BottomRight ->
                            updateThird (updateThird (Just player) bottom) grid

                _ ->
                    grid
    in
    List.foldl mark blankGrid (Behavior.log state)


updateFirst : a -> ( a, a, a ) -> ( a, a, a )
updateFirst new ( _, b, c ) =
    ( new, b, c )


updateSecond : a -> ( a, a, a ) -> ( a, a, a )
updateSecond new ( a, _, c ) =
    ( a, new, c )


updateThird : a -> ( a, a, a ) -> ( a, a, a )
updateThird new ( a, b, _ ) =
    ( a, b, new )


css : String
css =
    [ "body > div > div {"
    , "    display: contents;"
    , "}"
    , "body > div {"
    , "    display: grid;"
    , "    grid-template-columns: repeat(6,3rem);"
    , "    grid-template-rows: repeat(3,6rem) 1fr;"
    , "    padding: .5em;"
    , "    gap: .5em;"
    , "    background-color: coral;"
    , "    justify-content: center;"
    , "}"
    , "button {"
    , "    grid-column: span 2;"
    , "    font-size: 3rem;"
    , "    background: white;"
    , "    border: none;"
    , "}"
    , "ol {"
    , "    grid-column: span 3;"
    , "}"
    , "ol:nth-of-type(1)::before {"
    , "    content: 'Pending requests';"
    , "}"
    , "ol:nth-of-type(2)::before {"
    , "    content: 'Event log';"
    , "}"
    , "ol::before {"
    , "    font-size: .8em"
    , "}"
    ]
        |> String.join "\n"
