module InvoiceMailingTest exposing (..)

import Behavior exposing (..)
import Expect
import Set exposing (Set)
import Test exposing (..)


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
    [ Wait
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
    [ Wait
        (\event ->
            case event of
                Prepared invoice ->
                    if Set.member invoice.recipient previous then
                        Pause

                    else
                        Continue
                            [ andThen <| request (Sent <| firstTemplate invoice) []
                            , andThen <| block (Sent <| defaultTemplate invoice)
                            , firstInvoicesUseDifferentTemplate (Set.insert invoice.recipient previous)
                            ]

                _ ->
                    Pause
        )
    ]


suite : Test
suite =
    describe "Invoicing scenario"
        [ test "invoices get sent" <|
            \_ ->
                initialize [ sendInvoices ]
                    |> fire (Prepared { recipient = "Mert", amount = 42 })
                    |> log
                    |> Expect.equalLists
                        [ Sent "Hi Mert, your next invoice for $42 is ready"
                        , Prepared { recipient = "Mert", amount = 42 }
                        ]
        , test "first invoices get sent with a different template" <|
            \_ ->
                initialize [ sendInvoices, firstInvoicesUseDifferentTemplate Set.empty ]
                    |> fire (Prepared { recipient = "Mert", amount = 42 })
                    |> fire (Prepared { recipient = "Mert", amount = 100 })
                    |> log
                    |> Expect.equalLists
                        [ Sent "Hi Mert, your next invoice for $100 is ready"
                        , Prepared { recipient = "Mert", amount = 100 }
                        , Sent "Welcome Mert, your first invoice for $42 is ready"
                        , Prepared { recipient = "Mert", amount = 42 }
                        ]
        , test "everybody gets their own first emails" <|
            \_ ->
                initialize [ sendInvoices, firstInvoicesUseDifferentTemplate Set.empty ]
                    |> fire (Prepared { recipient = "Mert", amount = 42 })
                    |> fire (Prepared { recipient = "Mert", amount = 100 })
                    |> fire (Prepared { recipient = "Betül", amount = 2 })
                    |> fire (Prepared { recipient = "Mert", amount = 66 })
                    |> log
                    |> Expect.equalLists
                        [ Sent "Hi Mert, your next invoice for $66 is ready"
                        , Prepared { recipient = "Mert", amount = 66 }
                        , Sent "Welcome Betül, your first invoice for $2 is ready"
                        , Prepared { recipient = "Betül", amount = 2 }
                        , Sent "Hi Mert, your next invoice for $100 is ready"
                        , Prepared { recipient = "Mert", amount = 100 }
                        , Sent "Welcome Mert, your first invoice for $42 is ready"
                        , Prepared { recipient = "Mert", amount = 42 }
                        ]
        ]
