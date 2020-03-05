module TicTacToeApp exposing (main)

import Behavior
import Behavior.Program
import Html exposing (Html, button, div, li, ol, text)
import Html.Attributes exposing (reversed)
import Html.Events exposing (onClick)
import TicTacToe exposing (..)



-- PROGRAM


main : Program () StateAndGrid GameEvent
main =
    Behavior.Program.dualSandbox
        { init = init
        , view = view
        , update = update
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


init : StateAndGrid
init =
    ( behavior, blankGrid )



-- UPDATE


update : Behavior.State GameEvent -> GridMarks -> GridMarks
update state grid =
    List.foldl mark grid (Behavior.recent state)


mark : GameEvent -> GridMarks -> GridMarks
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



-- VIEW


view : StateAndGrid -> Html GameEvent
view ( state, grid ) =
    let
        ( ( topLeft, top, topRight ), ( left, center, right ), ( bottomLeft, bottom, bottomRight ) ) =
            grid
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


eventLi : GameEvent -> Html GameEvent
eventLi event =
    li [] [ text <| Debug.toString event ]


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
