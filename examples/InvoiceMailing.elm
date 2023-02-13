module InvoiceMailing exposing
    ( Invoice
    , InvoicingEvent(..)
    , firstInvoicesUseDifferentTemplate
    , sendInvoices
    )

import Behavior exposing (..)
import Set exposing (Set)


type InvoicingEvent
    = Prepared Invoice
    | Sent String


type alias Invoice =
    { recipient : String
    , amount : Int
    }


defaultTemplate : Invoice -> String
defaultTemplate invoice =
    String.concat
        [ "Hi "
        , invoice.recipient
        , ", your next invoice for $"
        , String.fromInt invoice.amount
        , " is ready"
        ]


firstTemplate : Invoice -> String
firstTemplate invoice =
    String.concat
        [ "Welcome "
        , invoice.recipient
        , ", your first invoice for $"
        , String.fromInt invoice.amount
        , " is ready"
        ]


sendInvoices : Thread InvoicingEvent
sendInvoices _ =
    [ wait
        (\event ->
            case event of
                Prepared invoice ->
                    Continue
                        [ andThen <| request (Sent <| defaultTemplate invoice) []
                        , sendInvoices
                        ]

                _ ->
                    Pause
        )
    ]


firstInvoicesUseDifferentTemplate : Set String -> Thread InvoicingEvent
firstInvoicesUseDifferentTemplate previous _ =
    [ wait
        (\event ->
            case event of
                Prepared invoice ->
                    if Set.member invoice.recipient previous then
                        Pause

                    else
                        Continue
                            [ andThen <| request (Sent <| firstTemplate invoice) []
                            , andThen <| blockEvent (Sent <| defaultTemplate invoice)
                            , firstInvoicesUseDifferentTemplate (Set.insert invoice.recipient previous)
                            ]

                _ ->
                    Pause
        )
    ]
