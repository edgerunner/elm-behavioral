module ThermostatTest exposing (..)

import Behavior exposing (..)
import Expect
import Test exposing (..)


type ThermostatEvent
    = Heater Bool
    | Temperature Int
    | Change Int
    | Setpoint Int
    | Tick


temperature : Int -> Thread ThermostatEvent
temperature temp _ =
    [ request (Temperature temp) [ currentTemperature temp, minimumTemperature 18, temperatureDrop 5 ] ]


currentTemperature : Int -> Thread ThermostatEvent
currentTemperature temp _ =
    [ wait
        (\event ->
            case event of
                Change delta ->
                    Continue
                        [ currentTemperature temp
                        , andThen <| request (Temperature (temp + delta)) []
                        ]

                Temperature t ->
                    Continue
                        [ currentTemperature t ]

                _ ->
                    Pause
        )
    ]


temperatureDrop : Int -> Thread ThermostatEvent
temperatureDrop ticks _ =
    if ticks == 0 then
        [ request (Change -1) [ temperatureDrop 5 ] ]

    else
        [ waitFor Tick [ temperatureDrop (ticks - 1) ] ]


minimumTemperature : Int -> Thread ThermostatEvent
minimumTemperature min _ =
    [ block
        (\event ->
            case event of
                Temperature temp ->
                    if temp < min then
                        Blocked

                    else
                        Free

                _ ->
                    Free
        )
    ]


heater : Int -> Thread ThermostatEvent
heater power _ =
    [ request (Heater False)
        [ heaterOff power, heaterDamage 200 ]
    ]


heaterOff : Int -> Thread ThermostatEvent
heaterOff power _ =
    [ waitFor (Heater True) [ heaterOn power ] ]


heaterOn : Int -> Thread ThermostatEvent
heaterOn power _ =
    [ waitFor (Heater False) [ heaterOff power ]
    , waitFor Tick
        [ heaterOn power
        , andThen <| request (Change power) []
        ]
    ]


heaterDamage : Int -> Thread ThermostatEvent
heaterDamage danger _ =
    [ wait
        (\event ->
            case event of
                Temperature t ->
                    if t > danger then
                        Continue
                            [ andThen <| request (Heater False) []
                            , andThen <| blockEvent (Heater True)
                            ]

                    else
                        Pause

                _ ->
                    Pause
        )
    ]


thermostat : Int -> Thread ThermostatEvent
thermostat setpoint _ =
    [ request (Setpoint setpoint)
        [ thermostatLoop setpoint ]
    ]


thermostatLoop : Int -> Thread ThermostatEvent
thermostatLoop setpoint _ =
    [ wait
        (\event ->
            case event of
                Temperature t ->
                    Continue
                        [ thermostatLoop setpoint
                        , andThen <| request (Heater <| t < setpoint) []
                        ]

                Setpoint s ->
                    Continue [ thermostatLoop s ]

                _ ->
                    Pause
        )
    ]


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
