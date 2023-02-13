module Thermostat exposing (temperature, heater, thermostat, ThermostatEvent(..))

import Behavior exposing (..)

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