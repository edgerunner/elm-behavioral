module Behavior exposing (..)


type alias Thread f e =
    f -> List (Behavior f e)


type Behavior f e
    = Request e (List f)
    | Wait (e -> WaitResult f)
    | Block (e -> BlockResult)


type WaitResult f
    = Continue (List f)
    | Pause


type BlockResult
    = Blocked
    | Free


type State f e
    = State (List ( Thread f e, f )) (List e)



-- PUBLIC API


initialize : List ( Thread f e, f ) -> State f e
initialize threads =
    State threads []
        |> run


run : State f e -> State f e
run state =
    case selectEvent (Debug.log "run" state) of
        Nothing ->
            state

        Just event ->
            applyEvent event state
                |> run



-- PRIVATE HELPERS


applyEvent : e -> State f e -> State f e
applyEvent event (State threads log) =
    threads
        |> List.concatMap (runBehaviors (Debug.log "event" event))
        |> (\newThreads -> State newThreads (event :: log))


runBehaviors : e -> ( Thread f e, f ) -> List ( Thread f e, f )
runBehaviors event ( thread, fiber ) =
    let
        behaviors =
            thread fiber |> Debug.log "behaviors"

        results =
            List.map (runBehavior event) behaviors

        unchanged =
            List.all ((==) Nothing) results

        newFibers =
            List.concatMap (Maybe.withDefault []) results
    in
    if unchanged then
        [ ( thread, fiber ) ]

    else
        List.map (\nextFiber -> ( thread, nextFiber )) newFibers


runBehavior : e -> Behavior f e -> Maybe (List f)
runBehavior event behavior =
    case behavior of
        Request e fibers ->
            if event == e then
                Just fibers

            else
                Nothing

        Wait fn ->
            case fn event of
                Continue fibers ->
                    Just fibers

                Pause ->
                    Nothing

        Block _ ->
            Nothing


blockChain : List ( Thread f e, f ) -> e -> Maybe e
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
        |> List.map applyLeft
        |> List.concatMap Tuple.second
        |> List.filterMap onlyBlocks
        |> List.foldl (<<) Just


selectEvent : State f e -> Maybe e
selectEvent (State threads _) =
    let
        blocks =
            blockChain threads

        requestedEvents =
            threads
                |> List.concatMap (Tuple.second << applyLeft)
                |> List.filterMap onlyRequestEvents

        onlyRequestEvents behavior =
            case behavior of
                Request evt _ ->
                    Just evt

                _ ->
                    Nothing
    in
    requestedEvents
        |> firstWith blocks



-- GENERIC HELPERS


applyLeft : ( a -> b, a ) -> ( a -> b, b )
applyLeft ( func, param ) =
    ( func, func param )


annexLeft : ( a, List b ) -> List ( a, b )
annexLeft ( a, list ) =
    List.map (\b -> ( a, b )) list


maybeIf : a -> Bool -> Maybe a
maybeIf item predicate =
    if predicate then
        Just item

    else
        Nothing


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
