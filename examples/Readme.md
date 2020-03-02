# Elm-behavioral

This is yet an **early 🌅 beta 🛴 WIP 🚧 unstable** 
attempt at implementing the thinking behind the paper 
[Behavioral Programming][paper] by David Harel, Assaf Marron and Gera Weiss 
in [Elm]

For a more complete attempt, see [react-behavioral] by Luca Matteis.

## Playing along

So if you want to fiddle with behaviors at this very early stage, 
read below at your own risk. For now, please read the 
[original article][paper] as well, so that the API makes 
a bit more sense.

## Events, threads & fibers

A behavioral system is made of independent `Thread`s (called *B-threads* in the original paper) of execution 
that are agnostic of each other, but listen and react to an agreed-upon
set of system events. A thread can ask the system to do one of three things:

- `Request` a system event to be published. 
  (Just a request, no guarantees)
- `Block` an event from getting published.
- `Wait` for an event and then change its state/behavior.

Since there are yet no proper communicating processes/threads in Elm,
I modeled threads as a series of functions that return a list of 
`Behavior` values to be interpreted by the system. Each one of these 
functions is called a *fiber*, representing one contiguous block of 
internal calculations before coming up with a new list of behaviors.  

The system then evaluates all the behaviors as a whole, and runs the
next batch of fibers if there are `Free` (ie. not `Blocked`) events in 
the requests. If there are no free events, the system returns its 
internal `State`, which can be re-invoked by external event 
(side effect).

One or more of these fiber functions, chained together through their 
returned behaviors, and possibly passing along some internal state, is 
what makes up a (B-)`Thread` 

## Tap control

Let's try the hot-cold water tap control example in the [paper]. One of 
the proposed advantages of behavioral programming is the ability to 
change system behavior without modifying the existing implementation,
just by adding new behaviors.

Of course begin by importing the `Behavior` module.

```elm
    import Behavior exposing (..)
```

The first thing to do is define our common set of events for the system.
Any data type in Elm can be used to describe an event, but getting the
most out of the excellent Elm compiler, I recommend using an union type
to define all the possible events.

```elm
type TapEvent
    = Hot
    | Cold
```

Not much here. Either a unit of `Hot` water flows, or `Cold`. 
Now we need a thread to make that happen. Let's also give it some
internal state for the amount of water we want.

```elm
addHot : Int -> Thread TapEvent
addHot amount _ =
    if amount > 0 then
        [ request Hot [ addHot (amount - 1) ] ]
    else
        []
```
Some points here
- The `_` in the function parameters is a stand-in for the 
empty tuple `()` elm-behavioral will pass in to lazily invoke it.
This is necessary to keep the compiler from crashing when you want 
to make a fiber recursively call itself indefinitely.
- The returned list is the list of behaviors to evaluate at the next 
*sync point*. `request` is the exposed constructor that takes an
*event* to be considered for publishing, and a list of fibers to 
invoke when that event is finally published. Here, `addHot` invokes 
itself with one less amount to go.
- The `else` case returns an empty list `[]` of behaviors, which
practically means this thread has nothing more to do. A thread is
over when it has no more behaviors, because it can't continue with
any fibers.

We `initialize` the system by giving it a list of fibers to begin with.
Here, we have just one thread. It immediately runs and evaluates all
the events it can. Once there are no more events that can be published,
it returns its `State`, which contains the remaining fibers, and a `log`
of events that can be extracted by the function of the same name.

```elm
initialize [ addHot 3 ] |> log --> [Hot, Hot, Hot]
```

It is also possible to `fire` events into the returned state. 
This generates and injects a one-shot fiber that just requests
a single event.

```elm
initialize [ addHot 3 ] 
    |> fire Cold
    |> log 
        --> [Cold, Hot, Hot, Hot]
```
Note that the last event is the first in the log. It works by prepending
the events as they happen.

### Adding another behavor

Let's just handwave the almost identical `addCold` thread into existence and focus on a new requirement. The problem is this is what happens
when we try to get warm water.

```elm
initialize [ addHot 3, addCold 3 ] 
    |> log 
        --> [Cold, Cold, Cold, Hot, Hot, Hot]
```
We are first scalded by hot water and then frozen. 🥵🥶

So the new requirement is that we should be able to mix taps. 

```elm
mix : TapEvent -> TapEvent -> Thread TapEvent
mix tap1 tap2 _ =
    [ blockEvent tap1
    , waitFor tap2 [ mix tap2 tap1 ]
    ]
```
Here is a new behavior that blocks one tap while waiting for the other,
at which point it re-invokes itself with the inverse configuration.

```elm
initialize [ addHot 3, addCold 3, mix Hot Cold ] 
    |> log 
        --> [Hot, Cold, Hot, Cold, Hot, Cold]
```
🛀 We can finally get a steady stream of water at the correct temperature.

For more examples, check out the [examples](./examples/) folder 
and read the tests.


[paper]: http://www.wisdom.weizmann.ac.il/~bprogram/
[Elm]: https://elm-lang.org/
[react-behavioral]: https://lmatteis.github.io/react-behavioral/