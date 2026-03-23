namespace Monads

// Monads.State provides the computation-expression wrapper used throughout the
// semantic pipeline.
//
// FSharpPlus exposes a State type, but not the builder and convenience surface
// used by the analyzer and pipeline layers. This module fills that gap and keeps
// state-threading code readable.

(*
   ================================================================
   State Monad Builder Module
   ================================================================
   
   This module provides a state monad that encapsulates computations
   that thread an immutable state through a series of operations. 
   F@+ defines State but does not provide a builder for it.
   
   Core Functions:
     - GetState() : Retrieves the current state.
     - PutState(newState) : Updates the state with a new value.
     - Return x : Lifts a pure value into the state monad.
     - Bind m f : Chains computations, passing the updated state along.
     - Combine m1 m2 : Executes m1, then m2, threading the state forward.
     - Delay f : Defers a computation, enabling lazy evaluation.
     
   Additional Utilities:
     - modify f : Applies a function to transform the state.
     - map f m : Applies a function to the result of a stateful computation.
     - mapState f m : Modifies the state without altering the computed result.
     - gets f : Extracts specific information from the current state.
     - sequence actions : Executes a list of stateful computations in order,
                           returning a list of their results.
   
   These operations facilitate composing complex stateful computations in a
   clear and functional manner.
*)

module State =
    open FSharpPlus
    open FSharpPlus.Data

    type StateBuilder() =
        // This builder is intentionally minimal: just enough to support the
        // explicit state-threaded workflows used by the compiler.
        member _.Return(x: 'T) : State<'S, 'T> =
            State(fun s -> (x, s))

        member _.Bind(m: State<'S, 'T>, f: 'T -> State<'S, 'U>) : State<'S, 'U> =
            State(fun s ->
                let (a, newState) = State.run m s
                State.run (f a) newState)

        member _.Combine(m1: State<'S, unit>, m2: State<'S, 'T>) : State<'S, 'T> =
            State(fun s ->
                let (_, newState) = State.run m1 s
                State.run m2 newState)

        member _.Delay(f: unit -> State<'S, 'T>) : State<'S, 'T> =
            State(fun s -> State.run (f()) s)

        member _.Zero() : State<'S, unit> =
            State(fun s -> ((), s))

    let state = StateBuilder()

    /// Retrieve the current state
    let getState : State<'S, 'S> =
        State.get

    /// Update the state with a new value
    let putState (newState: 'S) : State<'S, unit> =
        State.put newState

    /// Apply a function to transform the state
    let modify (f: 'S -> 'S) : State<'S, unit> =
        State.modify f

    /// Apply a function to the result of a computation
    let map (f: 'T -> 'U) (m: State<'S, 'T>) : State<'S, 'U> =
        State.map f m

    /// Extract specific information from the state without modifying it
    let gets (f: 'S -> 'T) : State<'S, 'T> =
        State.gets f

    /// Execute a list of stateful computations in sequence, discarding their results
    let sequence_ (actions: State<'S, unit> list) : State<'S, unit> =
        List.foldBack (fun action acc -> action |> State.bind (fun () -> acc)) actions (state { return () })

    /// Run multiple state computations sequentially
    let sequence (actions: State<'S, 'T> list) : State<'S, 'T list> =
        let cons a b = a :: b
        List.foldBack (fun action acc -> state {
            let! x = action
            let! xs = acc
            return cons x xs
        }) actions (state { return [] })

    /// Combines two stateful computations: executes m1 (which produces unit), then m2, threading the state.
    let combine (m1: State<'S, unit>) (m2: State<'S, 'T>) : State<'S, 'T> =
        state.Combine(m1, m2)
        
    /// Runs a state computation with an initial state
    let run (st: State<'S, 'T>) (init: 'S) : 'T * 'S =
        State.run st init
