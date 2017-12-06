module RepeatTests
open FsCheckGen
open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.Models.State
open System.Reflection.Metadata.Ecma335
let (==?) x y = 
    //printfn "%A" x
    //printfn "%A" y
    Expect.equal x y ""
type GamePrimitiveType = Scalar of string | List of GamePrimitiveType | Distr of GamePrimitiveType | Mixed | Unknown
let (|Sc|Ls|Ds|Mx|Uk|) = function 
        | Scalar s -> Sc s
        | List _ -> Ls
        | Distr _ -> Ds 
        | Mixed -> Mx
        | Unknown -> Uk
    

let rec toString = function 
    | Scalar s -> sprintf "Scalar<%s>" s 
    | List gpt -> sprintf "List<%s>" <| toString gpt
    | Distr gpt -> sprintf "Dist<%s>" <| toString gpt
    | Mixed -> "Mixed"
    | Unknown -> "Unknown"
let toTyped op = 
    let rec doCheck = function 
        | Int _                             -> Scalar "Int"  
        | Str(_)                            -> Scalar "Str"  
        | Float(_)                          -> Scalar "Float"
        | Check(Check.CheckValue(gp))       -> Scalar (doCheck gp |> toString) 
        | NoValue                           -> Unknown
        | ParamArray(ops)  -> 
            match List.distinctBy (function Value v -> doCheck v | _ ->  Unknown) ops with 
            | [] -> List (Unknown)
            | [Value(gp)]  -> doCheck gp |> List
            | _ -> Mixed
        | Tuple(a, b) -> Scalar (sprintf "Scalar<%s,%s>" (doCheck a |> toString) ((doCheck b |> toString)))
        | Dist(vs) -> 
            match List.distinctBy (fst >> doCheck) vs with 
            | [] -> Distr (Scalar "")
            | [gp,_]  -> doCheck gp |> Distr
            | _ -> Mixed
    match op with 
    | Value(gp) -> doCheck gp
    | _ -> Unknown


// vInt 6 |> toTyped |> toString
// [vInt 6; vInt 5] |> opList |> toTyped  |> toString
// [vInt 6; Value(Float(5.))] |> opList |> toTyped  |> toString
// Call Repeat |> toTyped  |> toString
// opList [] |> toTyped  |> toString
// [vInt 6; vInt 5] |> opList |> toTyped  |> toString
// let d = Distribution.uniformDistribution [1..6] |> Distribution.map Int |> GameActions.Primitives.Types.Dist |> Value
// let d2 = Distribution.uniformDistribution [1.0..6.0] |> Distribution.map Float |> GameActions.Primitives.Types.Dist |> Value
// d |> toTyped |> toString
// d2 |> toTyped |> toString
// [d;d] |> opList |> toTyped |> toString
// [[d;d] |> opList] |> opList |> toTyped |> toString
// [d;d2] |> opList |> toTyped |> toString
// [[d;d2] |> opList] |> opList |> toTyped |> toString
// [[d] |> opList; d2] |> opList |> toTyped |> toString
// [[d] |> opList; d] |> opList |> toTyped |> toString

let es x op = get x |> op |> evalOp standardCall Map.empty<_,_> 
let ea x op = get x |> op |> evalOp avgCall Map.empty<_,_> 
let e x op = get x |> op |> evalOp sampleCall Map.empty<_,_> 


[<Tests>]
let tests = 
    let ``Repeating an operation gives correct length`` (FsCheck.NonNegativeInt x) op = 
        let a = vInt x
        let repeat = repeatOp op a
        let (Value(ParamArray(result)))= repeat |> evalOp standardCall Map.empty<_,_>
        Expect.equal (List.length result) (max x 0) "Length of repeat should be same as length input, or 0 if < 1"
        match result with 
        | [] -> ()
        | head::tail -> Expect.allEqual tail head "All elements in repeat should be the same"
    let ``Repeat is same as List.init`` functionToCall operation = 
        let passOrFailCount = operation |> List.map (Value) |> opList |> call Count
        let times = 3
        let ``Three Passes`` = List.init times (fun _ -> passOrFailCount) |> opList |> call functionToCall >>= "ThreePasses"
        let ``Three Passes Repeat`` = repeat (Lam("_", passOrFailCount)) (vInt times) |> call functionToCall >>= "ThreePasses"
        (es  "ThreePasses" ``Three Passes``) ==? (es "ThreePasses" ``Three Passes Repeat``)
        (ea  "ThreePasses" ``Three Passes``) ==? (ea "ThreePasses" ``Three Passes Repeat``)
        (e   "ThreePasses" ``Three Passes``) ==? (e  "ThreePasses" ``Three Passes Repeat``)

    let ``Repeat Sum is the same as product`` x y =
        let x = Int 6 |> Distribution.always |> Dist |> Value
        let y = noValue
        let result = repeatOp x y |> call Total >>= "TotalSum" 
        let product = [x;y] |> opList |> call Product >>= "TotalProduct" 
        (result |> es "TotalSum") ==? (product |> es "TotalProduct")
        (result |> ea "TotalSum") ==? (product |> ea "TotalProduct")
        (result |> e  "TotalSum") ==? (product |> e  "TotalProduct")
    let ``NoValue + 3 = Unknown + T = T`` (value2:GamePrimitive) = 
        let value1 = NoValue
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = [Value value1;Value value2]  |> opList |> call Total >>= "result" |> es "result" |> toTyped
        value1Type ==? Unknown
        value2Type ==? result
    let ``3 + NoValue = T + Unknown = T`` (value1:GamePrimitive) = 
        let value2 = NoValue
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = [Value value1;Value value2]  |> opList |> call Total >>= "result" |> es "result" |> toTyped
        value1Type ==? Unknown
        value2Type ==? result
    let ``3 + 3 = Scalara + Scalara = Scalara`` (value1:GamePrimitive) (value2:GamePrimitive) = 
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = [Value value1;Value value2] |> opList |> call Total >>= "result" |> es "result" |> toTyped
        match value1Type,value2Type,result with 
        | Scalar a, Scalar a', Scalar a'' -> Expect.isTrue (a = a'' && a' = a'') "Scalars not same type"
        | a,b,c -> failwith <| sprintf "%A, %A, %A don't all have the same dimension" a b c
    let ``3 × 3 = Scalara × Scalara = Scalara`` (value1:GamePrimitive) (value2:GamePrimitive) = 
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = [Value value1;Value value2] |> opList |> call Product >>= "result" |> es "result" |> toTyped
        match value1Type,value2Type,result with 
        | Scalar a, Scalar a', Scalar a'' -> Expect.isTrue (a = a'' && a' = a'') "Scalars not same type"
        | a,b,c -> failwith <| sprintf "%A, %A, %A don't all have the same dimension" a b c
         
    let ``3 x 3 = Repeat 3 3 = Scalara x Scalarb = Scalara list`` (value1:GamePrimitive) (value2:GamePrimitive) = 
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = repeatOp (Value value1) (Value value2) >>= "result" |> es "result" |> toTyped
        match value1Type,value2Type,result with 
        | Scalar a, _, List (Scalar a'') -> Expect.isTrue (a = a'') "Scalars not same type"
        | a,b,c -> failwith <| sprintf "%A, %A, %A don't all have the same dimension" a b c
    let ``3D6 = Repeat D6 3 = Scalara Dist x Scalarb  = Scalara Dist List`` (value1:GamePrimitive) (value2:GamePrimitive) = 
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = repeatOp (Value value1) (Value value2) >>= "result" |> es "result" |> toTyped
        match value1Type,value2Type,result with 
        | Distr(Scalar a), _, List (Distr(Scalar a'')) -> Expect.isTrue (a = a'') "Scalars not same type"
        | a,b,c -> failwith <| sprintf "%A, %A, %A don't all have the same dimension" a b c
    let ``Double D6 = Scalara x Scalara Dist = Scalara Dist`` (value1:GamePrimitive) (value2:GamePrimitive) = 
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = [Value value1;Value value2] |> opList |> call Product >>= "result" |> es "result" |> toTyped
        match value1Type,value2Type,result with 
        | Scalar a, Distr(Scalar a'), Distr(Scalar a'') -> Expect.isTrue (a = a'' && a = a') "Scalars not same type"
        | a,b,c -> failwith <| sprintf "%A, %A, %A don't all have the same dimension" a b c
    let ``D3D6s = Repeat D6 D3 = Scalara Dist x Scalarb Dist = Scalara Dist List Dist`` (value1:GamePrimitive) (value2:GamePrimitive) = 
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = repeatOp (Value value1) (Value value2) >>= "result" |> es "result" |> toTyped
        match value1Type,value2Type,result with 
        | Distr(Scalar a), Distr(Scalar _), Distr(List(Distr(Scalar a''))) -> Expect.isTrue (a = a'') "Scalars not same type"
        | a,b,c -> failwith <| sprintf "%A, %A, %A don't all have the same dimension" a b c
    let ``D6 3 = Repeat 3 D6 = Scalara x Scalarb Dist = Scalara Dist List`` (value1:GamePrimitive) (value2:GamePrimitive) = 
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = repeatOp (Value value1) (Value value2) >>= "result" |> es "result" |> toTyped
        match value1Type,value2Type,result with 
        | Distr(Scalar a), Distr(Scalar _), List(Distr(Scalar a'')) -> Expect.isTrue (a = a'') "Scalars not same type"
        | a,b,c -> failwith <| sprintf "%A, %A, %A don't all have the same dimension" a b c
    
    testList "Repeat Tests" [
        testPropertyWithConfig config "Repeating an operation gives correct length" ``Repeating an operation gives correct length``
        testPropertyWithConfig config "Total - Repeat Op X Times equivalent to List init X Times "   <| ``Repeat is same as List.init`` Total
        testPropertyWithConfig config "Product - Repeat Op X Times equivalent to List init X Times " <| ``Repeat is same as List.init`` Product
        testPropertyWithConfig config "Count - Repeat Op X Times equivalent to List init X Times "   <| ``Repeat is same as List.init`` Count
        testPropertyWithConfig config "Total(Repeat x y) = Product(x,y)" ``Repeat Sum is the same as product``
        testPropertyWithConfig config "3 + 3 = Scalar + Scalar = Scalar" ``3 + 3 = Scalara + Scalara = Scalara``
        testPropertyWithConfig config "3 × 3 = Scalar × Scalar = Scalar " ``3 × 3 = Scalara × Scalara = Scalara``
        testPropertyWithConfig config "3 x 3 = Repeat 3 3 = Scalar x Scalar = Scalar list" ``3 x 3 = Repeat 3 3 = Scalara x Scalarb = Scalara list``
        testPropertyWithConfig config "3D6 = Repeat D6 3 = Scalara x Scalarb Dist = Scalarb Dist List" ``3D6 = Repeat D6 3 = Scalara Dist x Scalarb  = Scalara Dist List``
        testPropertyWithConfig config "Double D6 = Scalara x Scalara Dist = Scalara Dist" ``Double D6 = Scalara x Scalara Dist = Scalara Dist``
        testPropertyWithConfig config "D3D6s = Repeat D6 D3 = Scalara Dist x ScalarbDist = Dist Scalarb Dist List" ``D3D6s = Repeat D6 D3 = Scalara Dist x Scalarb Dist = Scalara Dist List Dist``
        testPropertyWithConfig config "D6 3 = Repeat 3 D6 = Scalarb Dist x Scalara Dist Scalara List" ``D6 3 = Repeat 3 D6 = Scalara x Scalarb Dist = Scalara Dist List``
        ]
