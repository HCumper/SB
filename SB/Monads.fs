namespace Monads

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
    
    // /// Runs a stateful computation with an initial state.
    // let run (st: State<'S, 'T>) (init: 'S) : ('T * 'S) =
    //     st init

    
    /// Runs a state computation with an initial state
    let run (st: State<'S, 'T>) (init: 'S) : 'T * 'S =
        State.run st init
    
    


// module StateResult =
//
//     /// A function: oldState -> (newState, Result<Value, Error>)
//     type StateResult<'S, 'T, 'E> = 'S -> 'S * Result<'T, 'E>
//
//     // let run (st: StateResult<'S, 'T, 'Error>) (init: 'S) : (Result<'T, 'Error> * 'S) =
//     //         st init
//     let run (comp: StateResult<'S,'T,'E>) (init: 'S) : ('S * Result<'T,'E>) =
//         comp init
//
//     /// Our builder to support "monadic" syntax
//     type StateResultBuilder<'S, 'E>() =
//
//         member _.Return(x : 'T) : StateResult<'S,'T,'E> =
//             fun state -> (state, Ok x)
//
//         /// Combine: run the first, and if successful, run the second.
//         member _.Combine
//             (m1: StateResult<'S, unit, 'E>,
//              m2: StateResult<'S, 'T, 'E>)
//             : StateResult<'S, 'T, 'E> =
//             fun s ->
//                 let (s1, r1) = m1 s
//                 match r1 with
//                 | Error e -> (s1, Error e)      // short-circuit
//                 | Ok ()   -> m2 s1
//         
//          /// 'Using' is needed if you want 'for x in xs do' or 'use!' syntax
//         member _.Using(resource: 'R, binder: 'R -> StateResult<'S,'T,'E>)
//             : StateResult<'S,'T,'E> when 'R :> System.IDisposable =
//             fun state ->
//                 try
//                     binder resource state
//                 finally
//                     // Dispose the resource after the monadic step
//                     if not (isNull (box resource)) then 
//                         resource.Dispose()
//
//         /// Called to delay execution of a lambda               
//         member _.Delay(f: unit -> StateResult<'S,'T,'E>) : StateResult<'S,'T,'E> =
//             fun s -> f() s
//         
//         member _.ReturnFrom(m : StateResult<'S,'T,'E>) : StateResult<'S,'T,'E> =
//             m
//
//         /// Overload so a result can be returned by a function expecting a StateResult
//         member _.ReturnFrom(r: Result<'T,'E>) : StateResult<'S,'T,'E> =
//             fun state -> (state, r)
//
//         /// Bind for StateResult<'S,'T1,'E>
//         member _.Bind
//             (m : StateResult<'S,'T1,'E>,
//              f : 'T1 -> StateResult<'S,'T2,'E>) : StateResult<'S,'T2,'E> =
//             fun oldState ->
//                 let (newState, result) = m oldState
//                 match result with
//                 | Error err -> (newState, Error err)    // short-circuits
//                 | Ok value  -> f value newState
//
//         /// Overload for binding a pure Result<'T,'E> into StateResult
//         member this.Bind
//             (r : Result<'T,'E>,
//              f : 'T -> StateResult<'S,'U,'E>) : StateResult<'S,'U,'E> =
//             fun st ->
//                 match r with
//                 | Error e -> (st, Error e)
//                 | Ok v    -> (this.Bind (this.Return v, f)) st
//
//         member _.Zero() : StateResult<'S, unit, 'E> =
//             fun st -> (st, Ok ())
//
//         /// Utility: get current state
//         member _.GetState() : StateResult<'S,'S,'E> =
//             fun st -> (st, Ok st)
//
//         /// Utility: set current state
//         member _.PutState(newSt : 'S) : StateResult<'S, unit, 'E> =
//             fun _ -> (newSt, Ok ())
//
//         /// Utility: Ignore a StateResult's return value, keeping only the state.
//         member _.Ignore (m: StateResult<'S, 'T, 'E>) : StateResult<'S, unit, 'E> =
//             fun st ->
//                 let (newState, _) = m st
//                 (newState, Ok ())
//
//     /// Create a single instance of the builder
//     let stateResult<'S,'E> = StateResultBuilder<'S,'E>()
//
//     /// Utility function: ignore a StateResult's return value.
//     let ignore (m: StateResult<'S, 'T, 'E>) : StateResult<'S, unit, 'E> =
//         stateResult.Ignore m
//
