module MyState

    type MyState<'S,'T> = 'S -> 'T * 'S
    
    let run (st: MyState<'S,'T>) (init: 'S) : ('T * 'S) =
        st init

    let map (f: 'T -> 'U) (st: MyState<'S,'T>) : MyState<'S,'U> =
        fun s ->
            let (x, s') = st s
            (f x, s')

    let bind (f: 'T -> MyState<'S,'U>) (st: MyState<'S,'T>) : MyState<'S,'U> =
        fun s ->
            let (x, s') = st s
            f x s'

    let getState : MyState<'S,'S> =
        fun s -> (s, s)

    let putState (newS : 'S) : MyState<'S, unit> =
        fun _ -> ((), newS)

    type StateBuilder() =
        member _.Return(x) : MyState<'S,'T> = fun s -> (x, s)
        member _.ReturnFrom(m: MyState<'S,'T>) = m
        member _.Bind(m: MyState<'S,'T>, f: 'T -> MyState<'S,'U>) =
            bind f m

    let state = StateBuilder()

    // Usage:
    let demo = state {
        let! st = getState
        do! putState (st + 1)
        return st
    }
