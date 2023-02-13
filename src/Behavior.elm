module Behavior exposing
    ( Behavior
    , BlockResult(..)
    , State
    , Thread
    , WaitResult(..)
    , andThen
    , block
    , blockEvent
    , fire
    , fireAll
    , fireOne
    , initialize
    , log
    , pending
    , recent
    , request
    , run
    , wait
    , waitFor
    )


type alias Thread e =
    () -> List (Behavior e)


type alias Threads e =
    List (Thread e)


type Behavior e
    = Request e (Threads e)
    | Wait (e -> WaitResult e)
    | Block (e -> BlockResult)


type WaitResult e
    = Continue (Threads e)
    | Pause


type BlockResult
    = Blocked
    | Free


type State e
    = State (Threads e) (List e) LastRun


type alias LastRun =
    Int



-- PUBLIC API


initialize : Threads e -> State e
initialize threads =
    State threads [] 0
        |> run


run : State e -> State e
run state =
    case selectEvent state of
        Nothing ->
            state

        Just event ->
            applyEvent event state
                |> run


fire : e -> State e -> State e
fire event (State threads eventLog _) =
    State (singleRequestThread event :: threads) eventLog 0 |> run


fireOne : List e -> State e -> State e
fireOne events (State threads eventLog _) =
    let
        thread _ =
            events
                |> List.map ((<|) Request >> (|>) [])
    in
    State (thread :: threads) eventLog 0 |> run


fireAll : List e -> State e -> State e
fireAll events state =
    case events of
        [] ->
            state

        first :: rest ->
            state
                |> fire first
                |> fireAll rest


log : State e -> List e
log (State _ eventLog _) =
    eventLog


recent : State e -> List e
recent (State _ eventLog lastRun) =
    List.take lastRun eventLog


pending : State e -> List e
pending (State threads _ _) =
    List.concatMap ((|>) ()) threads
        |> List.filterMap
            (\behavior ->
                case behavior of
                    Request event _ ->
                        Just event

                    _ ->
                        Nothing
            )


request : e -> Threads e -> Behavior e
request =
    Request


waitFor : e -> Threads e -> Behavior e
waitFor expected withNext =
    Wait
        (\received ->
            if expected == received then
                Continue withNext

            else
                Pause
        )


wait : (e -> WaitResult e) -> Behavior e
wait =
    Wait


blockEvent : e -> Behavior e
blockEvent expected =
    Block
        (\received ->
            if expected == received then
                Blocked

            else
                Free
        )


block : (e -> BlockResult) -> Behavior e
block =
    Block


andThen : Behavior e -> Thread e
andThen behavior () =
    [ behavior ]



-- PRIVATE HELPERS


singleRequestThread : e -> Thread e
singleRequestThread event () =
    [ Request event [] ]


applyEvent : e -> State e -> State e
applyEvent event (State threads eventLog lastRun) =
    threads
        |> List.concatMap (runBehaviors event)
        |> (\newThreads -> State newThreads (event :: eventLog) (lastRun + 1))


runBehaviors : e -> Thread e -> Threads e
runBehaviors event thread =
    let
        behaviors =
            thread ()

        results =
            List.map (runBehavior event) behaviors

        unchanged =
            List.all ((==) Nothing) results

        newThreads =
            List.concatMap (Maybe.withDefault []) results
    in
    if unchanged then
        [ thread ]

    else
        newThreads


runBehavior : e -> Behavior e -> Maybe (Threads e)
runBehavior event behavior =
    case behavior of
        Request e threads ->
            if event == e then
                Just threads

            else
                Nothing

        Wait fn ->
            case fn event of
                Continue threads ->
                    Just threads

                Pause ->
                    Nothing

        Block _ ->
            Nothing


blockChain : Threads e -> e -> Maybe e
blockChain threads =
    let
        chainable fn =
            Maybe.andThen <|
                \event ->
                    case fn event of
                        Blocked ->
                            Nothing

                        Free ->
                            Just event

        onlyBlocks behavior =
            case behavior of
                Block fn ->
                    Just (chainable fn)

                _ ->
                    Nothing
    in
    threads
        |> List.concatMap invoke
        |> List.filterMap onlyBlocks
        |> List.foldl (<<) Just


selectEvent : State e -> Maybe e
selectEvent (State threads _ _) =
    let
        blocks =
            blockChain threads

        requestedEvents =
            threads
                |> List.concatMap invoke
                |> List.filterMap onlyRequests

        onlyRequests behavior =
            case behavior of
                Request evt _ ->
                    Just evt

                _ ->
                    Nothing
    in
    requestedEvents
        |> firstWith blocks



-- GENERIC HELPERS


invoke : (() -> a) -> a
invoke fn =
    fn ()


firstWith : (a -> Maybe a) -> List a -> Maybe a
firstWith fn list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case fn head of
                Nothing ->
                    firstWith fn tail

                Just a ->
                    Just a
