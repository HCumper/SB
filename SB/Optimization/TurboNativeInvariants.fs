module TurboNativeInvariants

// Lazy loop-invariant row addresses. No operation is speculated: initialization
// occurs at the first original access, after its original column/row checks.
open HirTurboIrBackend
open TurboNativeIr

type Row = ConstantRow of int | ScalarRow of int
type Key = { Vector:int; Row:Row; Checks:int; Columns:int }
type Plan = { Rows:Key list; Leaves:Map<int,Map<int,ArrayRowCache>>; SaveBytes:int }
let empty = {Rows=[];Leaves=Map.empty;SaveBytes=0}

let plan (proof:NativeForLoopProof option) availableBytes hasCalls (leaves:(Block * Allocation) list) =
    match proof with
    | None -> empty // Serialized IR alone cannot establish descriptor/alias safety.
    | Some _ when hasCalls -> empty // No assumptions about helper register clobbers.
    | Some proof ->
        let writes=leaves |> List.collect(fun (ir,_) -> ir.Instructions) |> List.choose(function StoreVector(v,_) -> Some v | _ -> None) |> Set.ofList
        let candidates=leaves |> List.mapi(fun leaf (ir,allocation) ->
            let origins=ir.Instructions |> List.choose(function
                | Constant(v,n) -> Some(v,ConstantRow n)
                | LoadVector(v,vector) when vector<>proof.Counter && proof.InvariantScalarVectors.Contains vector && not(writes.Contains vector) -> Some(v,ScalarRow vector)
                | _ -> None) |> Map.ofList
            let rows=ir.Instructions |> List.choose(function
                | LoadArrayWord2(_,v,mask,columns,row,_,_) | StoreArrayWord2(v,mask,columns,row,_,_,_) ->
                    Some(if proof.InvariantArrayVectors.Contains v then origins.TryFind row |> Option.map(fun row -> {Vector=v;Row=row;Checks=mask;Columns=columns}) else None)
                | _ -> None)
            let locations=allocation.Instructions |> List.indexed |> List.choose(function
                | index,(LoadElement2 _ | StoreElement2 _) -> Some index
                | _ -> None)
            // Allocation must preserve the order and number of effectful accesses.
            if rows.Length<>locations.Length then []
            else List.zip locations rows |> List.choose(fun (index,key) -> key |> Option.map(fun key -> leaf,index,key))) |> List.concat
        // D5 holds validity bits; A3-A5 hold at most three complete long addresses.
        let count=min 3 (max 0 (availableBytes/4-1))
        let selected=candidates |> List.countBy(fun (_,_,key) -> key) |> List.sortBy(fun (key,uses) -> -uses,key) |> List.truncate count |> List.map fst
        if selected.IsEmpty then empty
        else
            let caches=selected |> List.mapi(fun i key -> key,{AddressRegister=3+i;ValidBit=i}) |> Map.ofList
            let locations=candidates |> List.choose(fun (leaf,index,key) -> caches.TryFind key |> Option.map(fun cache -> leaf,(index,cache)))
                          |> List.groupBy fst |> List.map(fun (leaf,items) -> leaf,items |> List.map snd |> Map.ofList) |> Map.ofList
            {Rows=selected;Leaves=locations;SaveBytes=4*(1+selected.Length)}
