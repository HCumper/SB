module StateResult

    open System
    
    /// Represents a stateful computation that may also fail.
    /// Given an initial state of type 'S, it returns a tuple: a result (of type Result<'T, 'Error>) 
    /// and an updated state.
    type StateResult<'S, 'T, 'Error> = 'S -> (Result<'T, 'Error> * 'S)

    /// Runs a state-result computation with an initial state.
    let run (st: StateResult<'S, 'T, 'Error>) (init: 'S) : (Result<'T, 'Error> * 'S) =
        st init

    /// Maps a function over the result of a state-result computation.
    let map (f: 'T -> 'U) (st: StateResult<'S, 'T, 'Error>) : StateResult<'S, 'U, 'Error> =
        fun s ->
            let (result, s') = st s
            (Result.map f result, s')

    /// Binds a function to the result of a state-result computation.
    let bind (f: 'T -> StateResult<'S, 'U, 'Error>) (st: StateResult<'S, 'T, 'Error>) : StateResult<'S, 'U, 'Error> =
        fun s ->
            let (result, s') = st s
            match result with
            | Ok v -> f v s'
            | Error e -> (Error e, s')

    /// Exposes the current state.
    let getState : StateResult<'S, 'S, 'Error> =
        fun s -> (Ok s, s)

    /// Updates the state.
    let putState (newS: 'S) : StateResult<'S, unit, 'Error> =
        fun _ -> (Ok (), newS)

    /// The computation expression builder for StateResult.
    type StateResultBuilder() =
        member _.Return(x: 'T) : StateResult<'S, 'T, 'Error> =
            fun s -> (Ok x, s)
        member _.ReturnFrom(m: StateResult<'S, 'T, 'Error>) : StateResult<'S, 'T, 'Error> = m
        member _.Bind(m: StateResult<'S, 'T, 'Error>, f: 'T -> StateResult<'S, 'U, 'Error>) : StateResult<'S, 'U, 'Error> =
            bind f m
        member _.Zero() : StateResult<'S, unit, 'Error> =
            fun s -> (Ok (), s)
        member _.Delay(f: unit -> StateResult<'S, 'T, 'Error>) : StateResult<'S, 'T, 'Error> =
            fun s -> f () s
        member _.Run(f: unit -> StateResult<'S, 'T, 'Error>) : StateResult<'S, 'T, 'Error> =
            f ()
        member _.TryWith(m: unit -> StateResult<'S, 'T, 'Error>, handler: exn -> StateResult<'S, 'T, 'Error>) : StateResult<'S, 'T, 'Error> =
            fun s ->
                try m () s
                with ex -> handler ex s
        member _.TryFinally(m: unit -> StateResult<'S, 'T, 'Error>, compensation: unit -> unit) : StateResult<'S, 'T, 'Error> =
            fun s ->
                try m () s
                finally compensation ()
        member _.Using(resource: 'T when 'T :> IDisposable, body: 'T -> StateResult<'S, 'U, 'Error>) : StateResult<'S, 'U, 'Error> =
            fun s ->
                try body resource s
                finally resource.Dispose()
        member this.While(condition: unit -> bool, body: unit -> StateResult<'S, unit, 'Error>) : StateResult<'S, unit, 'Error> =
            fun s ->
                let rec loop s =
                    if condition () then
                        let (result, s') = body () s
                        match result with
                        | Ok () -> loop s'
                        | Error e -> (Error e, s')
                    else (Ok (), s)
                loop s
        member _.For(sequence: seq<'T>, body: 'T -> StateResult<'S, unit, 'Error>) : StateResult<'S, unit, 'Error> =
            fun s ->
                let rec loop (enum: System.Collections.Generic.IEnumerator<'T>) s =
                    if enum.MoveNext() then
                        let (result, s') = body enum.Current s
                        match result with
                        | Ok () -> loop enum s'
                        | Error e -> (Error e, s')
                    else (Ok (), s)
                use enumerator = sequence.GetEnumerator()
                loop enumerator s

    /// Instantiate the StateResult builder.
    let stateResult = StateResultBuilder()
