module ThermostatTest exposing (suite)

import Behavior exposing (..)
import Expect
import Test exposing (..)
import Thermostat exposing (..)


suite : Test
suite =
    describe "Thermostat"
        [ test "temperature drops by 1 every 5 ticks" <|
            \_ ->
                initialize [ temperature 30 ]
                    |> timePasses 10
                    |> checkTemperature 28
        , test "temperature doesn't go below 18" <|
            \_ ->
                initialize [ temperature 20 ]
                    |> timePasses 15
                    |> checkTemperature 18
        , test "heater increases temperature when on" <|
            \_ ->
                initialize [ heater 1, temperature 30 ]
                    |> fire (Heater True)
                    |> timePasses 15
                    |> fire (Heater False)
                    |> timePasses 10
                    |> checkTemperature 40
        , test "heater is destroyed above 200°" <|
            \_ ->
                initialize [ heater 50, temperature 30 ]
                    |> fire (Heater True)
                    |> timePasses 5
                    |> fire (Heater True)
                    |> timePasses 10
                    |> checkTemperature 227
        , test "thermostat keeps temperature near setpoint" <|
            \_ ->
                initialize [ thermostat 65, heater 2, temperature 30 ]
                    |> timePasses 25
                    |> log
                    |> List.filterMap
                        (\event ->
                            case event of
                                Temperature t ->
                                    Just t

                                _ ->
                                    Nothing
                        )
                    |> List.head
                    |> Maybe.withDefault 0
                    |> Expect.all
                        [ Expect.atLeast 64
                        , Expect.atMost 66
                        ]
        ]



-- Test helpers


timePasses : Int -> State ThermostatEvent -> State ThermostatEvent
timePasses t =
    fireAll (List.repeat t Tick)


checkTemperature : Int -> State ThermostatEvent -> Expect.Expectation
checkTemperature temp state =
    state
        |> log
        |> List.filter
            (\event ->
                case event of
                    Temperature _ ->
                        True

                    _ ->
                        False
            )
        |> List.head
        |> Expect.equal (Just <| Temperature temp)
