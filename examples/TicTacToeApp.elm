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
        , behavior = singlePlayer
        }



-- VIEW


view : State GameEvent -> Html GameEvent
view state =
    let
        { topLeft, top, topRight, left, center, right, bottomLeft, bottom, bottomRight } =
            extractGrid state
    in
    div [ state |> turn |> turnClass ]
        [ Css.Global.global [ style ] |> Html.Styled.toUnstyled
        , div [] <| List.map2 cell [ TopLeft, Top, TopRight ] [ topLeft, top, topRight ]
        , div [] <| List.map2 cell [ Left, Center, Right ] [ left, center, right ]
        , div [] <| List.map2 cell [ BottomLeft, Bottom, BottomRight ] [ bottomLeft, bottom, bottomRight ]
        , state |> endgame |> endgameView
        ]


turnClass : Maybe Player -> Html.Attribute e
turnClass turn =
    class <|
        case turn of
            Nothing ->
                "gameover"

            Just X ->
                "turn-x"

            Just O ->
                "turn-o"


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
                >> div [ class "endgame", onClick Restart ]
            )
        >> Maybe.withDefault (text "")


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
        [ Css.boxSizing Css.borderBox
        , Css.fontFamilies [ "Lato", "Verdana", Css.sansSerif.value ]
        , Css.Global.children
            [ Css.Global.div
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
                , Css.Global.children
                    [ Css.Global.div
                        [ Css.property "display" "contents" ]
                    , Css.Global.class "endgame"
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
                        ]
                    ]
                , Css.Global.descendants
                    [ Css.Global.button
                        [ Css.fontSize (Css.rem 3)
                        , Css.backgroundColor palette.white
                        , Css.borderStyle Css.none
                        , Css.Global.withClass "highlighted"
                            [ Css.backgroundColor palette.black
                            , Css.color palette.white
                            ]
                        ]
                    ]
                , Css.Global.withClass "turn-x"
                    [ Css.Global.descendants
                        [ Css.Global.button
                            [ Css.Global.withClass "blank"
                                [ Css.after
                                    [ Css.property "content" "'X'"
                                    , Css.opacity (Css.num 0.05)
                                    ]
                                , Css.hover
                                    [ Css.after
                                        [ Css.opacity (Css.num 0.4) ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , Css.Global.withClass "turn-o"
                    [ Css.Global.descendants
                        [ Css.Global.button
                            [ Css.Global.withClass "blank"
                                [ Css.after
                                    [ Css.property "content" "'O'"
                                    , Css.opacity (Css.num 0.05)
                                    ]
                                , Css.hover
                                    [ Css.after
                                        [ Css.opacity (Css.num 0.4) ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
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
