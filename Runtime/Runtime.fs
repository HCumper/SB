module Runtime

open System.Threading

(* Presents an interface
type IRuntime =
    abstract member declareLocal: name: string -> initial: Value -> unit
    abstract member declareGlobal: name: string -> initial: Value -> unit
    abstract member pushFrame: unit -> unit
    abstract member popFrame: unit -> unit
    abstract member callFunction: functionName: string -> formalParam: string -> actualArg: string -> body: (unit -> unit) -> unit

or in C#
public interface IRuntime
{
    /// <summary>
    /// Declares a new local variable in the current frame.
    /// </summary>
    /// <param name="name">The variable name.</param>
    /// <param name="initial">The initial value.</param>
    void DeclareLocal(string name, Value initial);

    /// <summary>
    /// Declares a new global variable.
    /// </summary>
    /// <param name="name">The variable name.</param>
    /// <param name="initial">The initial value.</param>
    void DeclareGlobal(string name, Value initial);

    /// <summary>
    /// Pushes a new frame onto the environment stack.
    /// </summary>
    void PushFrame();

    /// <summary>
    /// Pops the top frame from the environment stack.
    /// </summary>
    void PopFrame();

    /// <summary>
    /// Simulates a function call by using the given parameters and executing the function body.
    /// </summary>
    /// <param name="functionName">The name of the function (for debugging purposes).</param>
    /// <param name="formalParam">The formal parameter name.</param>
    /// <param name="actualArg">The actual argument name.</param>
    /// <param name="body">The function body to execute.</param>
    void CallFunction(string functionName, string formalParam, string actualArg, Action body);
}

*)

/// This class hierarchy is tacky but C# has a conniption when asked to access F# discriminated unions.
[<AbstractClass>]
type Value() =
    static member Integer(n: int) = IntegerValue(n) :> Value
    static member String(s: string) = StringValue(s) :> Value
    static member Real(r: float) = RealValue(r) :> Value

and IntegerValue(n: int) =
    inherit Value()
    member _.N = n
    override this.ToString() = $"Integer %d{n}"

and StringValue(s: string) =
    inherit Value()
    member _.Text = s
    override this.ToString() = $"String \"%s{s}\""

and RealValue(r: float) =
    inherit Value()
    member _.R = r
    override this.ToString() = $"Real %f{r}"

/// And VarCell stays as a record with a mutable field:
type VarCell = { mutable value: Value }

/// A frame is a group of mappings from variable name to VarCell reference
type Frame = Map<string, VarCell>

/// Our runtime environment is a stack of frames, topmost first.
/// The bottommost (last) in the list is the global frame.
type EnvStack = Frame list

/// Create an empty frame
let emptyFrame : Frame = Map.empty

/// Thread-local environment: each thread gets its own EnvStack,
/// initialized to a single empty (global) frame.
let private threadLocalEnv =
    new ThreadLocal<EnvStack>(fun () -> [ emptyFrame ])

/// Helper functions for getting/setting the current thread’s EnvStack
let private currentEnv() = threadLocalEnv.Value
let private setEnv (env: EnvStack) = threadLocalEnv.Value <- env
// Look up a variable by name, scanning from top to bottom.
/// If found, returns Some VarCell; otherwise, None.
let rec lookupVar (name: string) : VarCell option =
    let env = currentEnv()
    match env with
    | [] -> None
    | frame :: rest ->
        match frame.TryFind(name) with
        | Some cell -> Some cell
        | None ->
            // Temporarily set the thread-local env to rest for recursion
            threadLocalEnv.Value <- rest
            let result = lookupVar name
            // Restore environment
            threadLocalEnv.Value <- env
            result

/// Create a new VarCell with an initial value.
let private createVar (initial: Value) : VarCell =
    { value = initial }

/// Declare a local variable in the topmost frame.
/// If there are no frames, a new frame is created.
let declareLocal (name: string) (initial: Value) : unit =
    let env = currentEnv()
    let updatedEnv =
        match env with
        | [] ->
            let cell = createVar initial
            let newFrame = Map [ name, cell ]
            [ newFrame ]
        | topFrame :: rest ->
            let cell = createVar initial
            let updatedTop = topFrame.Add(name, cell)
            updatedTop :: rest
    setEnv updatedEnv

/// Declare a global variable (in the bottommost frame). Also ensure that every frame in between
/// has a reference to the same VarCell.
let declareGlobal (name: string) (initial: Value) : unit =
    let env = currentEnv()
    match List.rev env with
    | [] ->
        // No frames: create a global frame.
        let globalCell = createVar initial
        let globalFrame = Map [ name, globalCell ]
        setEnv [ globalFrame ]
    | globalFrame :: others ->
        let cell = createVar initial
        let updatedGlobal = globalFrame.Add(name, cell)
        // Propagate the reference to all intermediate frames.
        let rec propagate (frames: Frame list) (acc: Frame list) =
            match frames with
            | [] -> List.rev acc
            | f :: fs ->
                let f2 =
                    if f.ContainsKey(name) then f
                    else f.Add(name, cell)
                propagate fs (f2 :: acc)
        let newGlobalToTop = propagate (updatedGlobal :: others) []
        setEnv (List.rev newGlobalToTop)

/// Push a new frame onto the environment stack (e.g. when entering a function call).
let pushFrame () : unit =
    let env = currentEnv()
    match env with
    | [] -> setEnv [ emptyFrame ]
    | top :: _ ->
        // For simplicity, we reuse the top frame.
        // (If isolation is needed, you could copy the frame instead.)
        let newFrame = top
        setEnv (newFrame :: env)

/// Pop the top frame from the environment stack (e.g. on function return).
let popFrame () : unit =
    let env = currentEnv()
    match env with
    | [] -> ()
    | _ :: rest -> setEnv rest

/// Simulate a function call.
/// If the formal parameter differs from the actual argument,
/// create a new local variable in the new frame copying the actual argument's value.
/// Then run the function body and finally pop the frame.
let callFunction
    (functionName: string)
    (formalParam: string)
    (actualArg: string)
    (body: unit -> unit) : unit =

    pushFrame ()
    if actualArg <> formalParam then
        let maybeVar = lookupVar actualArg
        let paramValue =
            match maybeVar with
            | Some cell -> cell.value
            | None -> Value.Integer 0  // default if not found
        declareLocal formalParam paramValue
    body()
    popFrame ()
    
let getInt (value: string) : int =
    (lookupVar(value).Value.value :?> IntegerValue).N
    
let getString (value: string) : string =
    (lookupVar(value).Value.value :?> StringValue).Text
    
let getFloat (value: string) : float =
    (lookupVar(value).Value.value :?> RealValue).R
    
    
// ---------------------------------------------------------------------------
// Example usage (uncomment to test in a project):
(*
[<EntryPoint>]
let main _ =
    // 1) Declare a global variable x = 10
    declareGlobal "x" (Integer 10)

    // 2) Define a small body that increments its parameter by some delta
    let incVar varName delta =
        match lookupVar varName with
        | Some cell ->
            match cell.value with
            | Integer n -> cell.value <- Integer (n + delta)
            | _ -> ()
        | None ->
            printfn "Variable '%s' not found" varName

    // 3) Call a function f(p) with actual argument x
    //    Since "p" != "x", a new local 'p' gets created from x's value
    callFunction "f" "p" "x" (fun () ->
        incVar "p" 5
    )

    // After returning, local 'p' is gone. Let's see if 'x' changed
    match lookupVar "x" with
    | Some cell -> printfn "x after f(x) = %A" cell.value
    | None -> printfn "x not found"

    // 4) Another call g(x) with actualArg="x", formalParam="x"
    //    No new local is created, so changes go to the same cell as global 'x'.
    callFunction "g" "x" "x" (fun () ->
        incVar "x" 10
    )

    // Check if global 'x' was modified
    match lookupVar "x" with
    | Some cell -> printfn "x after g(x) = %A" cell.value
    | None -> printfn "x not found"

    0
*)

module InteropWrapper =

    open System

    type public Wrapper =
        // For void-returning operations
        static member Call(action: Action) =
            pushFrame()
            try
                action.Invoke()
            finally
                popFrame()

        // For value-returning operations
        static member Call<'T>(func: Func<'T>) =
            pushFrame()
            try
                func.Invoke()
            finally
                popFrame()