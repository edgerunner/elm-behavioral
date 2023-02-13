module TicTacToeApp exposing (main)

import Behavior exposing (State)
import Behavior.Program
import Css exposing (Style)
import Css.Global exposing (Snippet)
import Css.Transitions
import Html.Styled as Html exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import TicTacToe exposing (Cell(..), GameEvent(..), Grid, Mark(..), Player(..))



-- PROGRAM


main : Program () (State GameEvent) GameEvent
main =
    Behavior.Program.sandbox
        { view = view >> Html.toUnstyled
        , behavior = TicTacToe.singlePlayer
        }



-- VIEW


view : State GameEvent -> Html GameEvent
view state =
    let
        { topLeft, top, topRight, left, center, right, bottomLeft, bottom, bottomRight } =
            extractGrid state

        rowView positions marks =
            List.map2 (state |> TicTacToe.turn |> cellView) positions marks
                |> div [ css [ rowStyle ] ]
    in
    div [ css [ gameStyle ] ]
        [ Css.Global.global [ globalCSS ]
        , rowView [ TopLeft, Top, TopRight ] [ topLeft, top, topRight ]
        , rowView [ Left, Center, Right ] [ left, center, right ]
        , rowView [ BottomLeft, Bottom, BottomRight ] [ bottomLeft, bottom, bottomRight ]
        , state |> TicTacToe.endgame |> endgameView
        ]


endgameView : Maybe GameEvent -> Html GameEvent
endgameView =
    let
        endgameText event =
            case event of
                Win X _ _ _ ->
                    Just "X won!"

                Win O _ _ _ ->
                    Just "O won!"

                Tie ->
                    Just "It's a tie"

                _ ->
                    Nothing
    in
    Maybe.andThen endgameText
        >> Maybe.map
            (text
                >> List.singleton
                >> div [ css [ endgameStyle ], onClick Restart ]
            )
        >> Maybe.withDefault (text "")


extractGrid : State GameEvent -> Grid
extractGrid state =
    let
        latestBoardUpdate eventLog =
            case eventLog of
                [] ->
                    TicTacToe.empty

                (Board grid) :: _ ->
                    grid

                _ :: rest ->
                    latestBoardUpdate rest
    in
    latestBoardUpdate <| Behavior.log state


cellView : Maybe Player -> Cell -> Mark -> Html GameEvent
cellView turn currentCell cellMark =
    let
        ( markChar, markStyles ) =
            case cellMark of
                Blank ->
                    ( turn
                        |> Maybe.map playerMark
                        |> Maybe.withDefault ""
                    , [ cellStyle
                      , blankCellStyles
                      , turn
                            |> Maybe.map (\_ -> Css.cursor Css.pointer)
                            |> Maybe.withDefault (Css.cursor Css.unset)
                      ]
                    )

                Marked player ->
                    ( playerMark player, [ cellStyle, computerPlayerTransitionDelay player ] )

                Highlighted player ->
                    ( playerMark player, [ cellStyle, highlightedCellStyle ] )
    in
    button [ onClick (Click currentCell), css markStyles ] [ text markChar ]


playerMark : Player -> String
playerMark player =
    case player of
        X ->
            "X"

        O ->
            "O"


gameStyle : Style
gameStyle =
    Css.batch
        [ Css.property "display" "grid"
        , Css.property "grid-template-columns" "repeat(3,6rem)"
        , Css.property "grid-template-rows" "repeat(3,6rem)"
        , Css.padding (Css.em 0.5)
        , Css.property "gap" "0.5em"
        , Css.justifyContent Css.center
        , Css.property "align-content" "center"
        , Css.backgroundColor palette.coral
        , Css.height (Css.vh 100)
        , Css.boxSizing Css.borderBox
        , Css.position Css.relative
        ]


rowStyle : Style
rowStyle =
    Css.batch [ Css.property "display" "contents" ]


endgameStyle : Style
endgameStyle =
    Css.batch
        [ Css.position Css.absolute
        , Css.displayFlex
        , Css.fontSize (Css.em 2)
        , Css.textAlign Css.center
        , Css.property "inset" "0.5em"
        , Css.property "grid-row" "2/2"
        , Css.property "grid-column" "1/-1"
        , Css.margin Css.auto
        , Css.alignItems Css.center
        , Css.justifyContent Css.center
        , Css.backgroundColor palette.bisque
        , Css.padding2 (Css.rem 0.5) (Css.rem 1)
        , Css.borderRadius (Css.rem 1)
        , Css.border3 (Css.rem 0.5) Css.solid palette.coral
        , Css.cursor Css.pointer
        , Css.after
            [ Css.property "content" "'\\00a0⎋'"
            , Css.Transitions.transition
                [ Css.Transitions.color 100 ]
            , Css.color palette.white
            , Css.hover
                [ Css.color palette.coral ]
            ]
        ]


cellStyle : Style
cellStyle =
    Css.batch
        [ Css.fontSize (Css.rem 3)
        , Css.borderStyle Css.none
        , Css.backgroundColor palette.white
        , Css.color palette.black
        , Css.Transitions.transition
            [ Css.Transitions.color 100 ]
        ]


highlightedCellStyle : Style
highlightedCellStyle =
    Css.batch
        [ Css.backgroundColor palette.black
        , Css.color palette.white
        ]


blankCellStyles : Style
blankCellStyles =
    Css.batch
        [ Css.color palette.bisque
        , Css.hover
            [ Css.color palette.coral ]
        ]


computerPlayerTransitionDelay : Player -> Style
computerPlayerTransitionDelay player =
    Css.Transitions.transition <|
        case player of
            O ->
                [ Css.Transitions.color2 100 200 ]

            X ->
                []


globalCSS : Snippet
globalCSS =
    Css.Global.body
        [ Css.boxSizing Css.borderBox
        , Css.fontFamilies [ "Lato", "Verdana", Css.sansSerif.value ]
        ]


palette :
    { white : Css.Color
    , black : Css.Color
    , coral : Css.Color
    , bisque : Css.Color
    }
palette =
    { white = Css.rgb 255 255 255
    , black = Css.rgb 0 0 0
    , coral = Css.rgb 255 127 80
    , bisque = Css.rgb 255 228 196
    }
