module InvoiceMailingTest exposing (suite)

import Behavior exposing (..)
import Expect
import InvoiceMailing exposing (..)
import Set
import Test exposing (..)


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
