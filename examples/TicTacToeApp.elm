module TicTacToeApp exposing (main)

import Behavior
import Behavior.Program
import Html exposing (Html, button, div, text)
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


type alias RowMarks =
    ( Maybe Player, Maybe Player, Maybe Player )


type alias GridMarks =
    ( RowMarks, RowMarks, RowMarks )


blankGrid : GridMarks
blankGrid =
    ( ( Nothing, Nothing, Nothing )
    , ( Nothing, Nothing, Nothing )
    , ( Nothing, Nothing, Nothing )
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
reduce event (( top, mid, bottom ) as grid) =
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


cell : Cell -> Maybe Player -> Html GameEvent
cell currentCell player =
    let
        markChar =
            case player of
                Nothing ->
                    "-"

                Just X ->
                    "X"

                Just O ->
                    "O"
    in
    button [ onClick (Click currentCell) ] [ text markChar ]


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
    , "    grid-template-columns: repeat(3,6rem);"
    , "    grid-template-rows: repeat(3,6rem);"
    , "    padding: .5em;"
    , "    gap: .5em;"
    , "    background-color: coral;"
    , "    justify-content: center;"
    , "}"
    , "button {"
    , "    font-size: 3rem;"
    , "    background: white;"
    , "    border: none;"
    , "}"
    ]
        |> String.join "\n"
