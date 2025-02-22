module StateResult

/// A function: oldState -> (newState, Result<Value, Error>)
type StateResult<'S, 'T, 'E> = 'S -> 'S * Result<'T, 'E>

// let run (st: StateResult<'S, 'T, 'Error>) (init: 'S) : (Result<'T, 'Error> * 'S) =
//         st init
let run (comp: StateResult<'S,'T,'E>) (init: 'S) : ('S * Result<'T,'E>) =
    comp init

/// Our builder to support "monadic" syntax
type StateResultBuilder<'S, 'E>() =

    member _.Return(x : 'T) : StateResult<'S,'T,'E> =
        fun state -> (state, Ok x)

    /// Combine: run the first, and if successful, run the second.
    member _.Combine
        (m1: StateResult<'S, unit, 'E>,
         m2: StateResult<'S, 'T, 'E>)
        : StateResult<'S, 'T, 'E> =
        fun s ->
            let (s1, r1) = m1 s
            match r1 with
            | Error e -> (s1, Error e)      // short-circuit
            | Ok ()   -> m2 s1
    
     /// 'Using' is needed if you want 'for x in xs do' or 'use!' syntax
    member _.Using(resource: 'R, binder: 'R -> StateResult<'S,'T,'E>)
        : StateResult<'S,'T,'E> when 'R :> System.IDisposable =
        fun state ->
            try
                binder resource state
            finally
                // Dispose the resource after the monadic step
                if not (isNull (box resource)) then 
                    resource.Dispose()

    /// Called to delay execution of a lambda               
    member _.Delay(f: unit -> StateResult<'S,'T,'E>) : StateResult<'S,'T,'E> =
        fun s -> f() s
    
    member _.ReturnFrom(m : StateResult<'S,'T,'E>) : StateResult<'S,'T,'E> =
        m

    /// overload so a result can be returned by a funtion expecting a StateResult
    member _.ReturnFrom(r: Result<'T,'E>) : StateResult<'S,'T,'E> =
        fun state -> (state, r)

    /// Bind for StateResult<'S,'T1,'E>
    member _.Bind
        (m : StateResult<'S,'T1,'E>,
         f : 'T1 -> StateResult<'S,'T2,'E>) : StateResult<'S,'T2,'E> =
        fun oldState ->
            let (newState, result) = m oldState
            match result with
            | Error err -> (newState, Error err)    // short-circuits
            | Ok value  -> f value newState

    /// Overload for binding a pure Result<'T,'E> into StateResult
    member this.Bind
        (r : Result<'T,'E>,
         f : 'T -> StateResult<'S,'U,'E>) : StateResult<'S,'U,'E> =
        fun st ->
            match r with
            | Error e -> (st, Error e)
            | Ok v    -> (this.Bind (this.Return v, f)) st

    member _.Zero() : StateResult<'S, unit, 'E> =
        fun st -> (st, Ok ())

    /// Utility: get current state
    member _.GetState() : StateResult<'S,'S,'E> =
        fun st -> (st, Ok st)

    /// Utility: set current state
    member _.PutState(newSt : 'S) : StateResult<'S, unit, 'E> =
        fun _ -> (newSt, Ok ())

/// Create a single instance of the builder
let stateResult<'S,'E> = StateResultBuilder<'S,'E>()




open System
