open System

/// Node types for demonstration
type NodeType =
    | Root
    | Child
    | Grandchild

/// A tree node has a type, a value, and a list of children
type Node = {
    Type: NodeType
    Value: string
    Children: Node list
}

/// A listener function takes the accumulated strings so far 
/// and the node being exited, and returns an updated string list.
type Listener = string list -> Node -> string list

/// Define "exit" listeners for each node type
let rootListener: Listener =
    fun acc node -> acc @ [ sprintf "Exiting ROOT node with value: %s" node.Value ]

let childListener: Listener =
    fun acc node -> acc @ [ sprintf "Exiting CHILD node with value: %s" node.Value ]

let grandchildListener: Listener =
    fun acc node -> acc @ [ sprintf "Exiting GRANDCHILD node with value: %s" node.Value ]

/// Register listeners in a map keyed by NodeType
let exitListeners: Map<NodeType, Listener> =
    Map.ofList [
        Root,        rootListener
        Child,       childListener
        Grandchild,  grandchildListener
    ]

/// Walks the tree in post-order: visit children first, then "exit" the node
let walkTree (node: Node) : string list =
    let rec walk node soFar =
        // First, recursively walk children (post-order)
        let soFar = List.foldBack walk node.Children soFar
        // Then, invoke an exit listener if it exists
        match Map.tryFind node.Type exitListeners with
        | Some listener -> listener soFar node
        | None -> soFar

    // Start walking from the given node with an empty list
    walk node []

/// Example usage
[<EntryPoint>]
let main _ =
    // A small sample tree to demonstrate
    let sampleTree =
        { Type = Root; Value = "root"; Children =
            [ { Type = Child; Value = "child1"; Children = [] }
              { Type = Child; Value = "child2"; Children =
                    [ { Type = Grandchild; Value = "grandchild1"; Children = [] } ] } ] }

    // Traverse the tree and collect exit messages
    let results = walkTree sampleTree

    // Print the accumulated results
    results |> List.iter Console.WriteLine

    0
