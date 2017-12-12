module TypeChecker
open FsCheckGen
open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.Models.State
let (==?) x y = 
    //printfn "%A" x
    //printfn "%A" y
    Expect.equal x y ""
type GamePrimitiveType = Scalar of string | List of GamePrimitiveType | Distr of GamePrimitiveType | Empty | Mixed | Unknown
let rec toString = function 
    | Scalar s -> sprintf "Scalar<%s>" s 
    | List gpt -> sprintf "List<%s>" <| toString gpt
    | Distr gpt -> sprintf "Dist<%s>" <| toString gpt
    | Mixed -> "Mixed"
    | Unknown -> "Unknown"
    | Empty -> "Empty"
let toTyped op = 
    let rec doCheck = function 
        | Int _                             -> Scalar "Int"  
        | Str(_)                            -> Scalar "Str"  
        | Float(_)                          -> Scalar "Float"
        | Check(Check.CheckValue(gp))       -> Scalar (doCheck gp |> toString) 
        | NoValue                           -> Unknown
        | ParamArray(ops)  -> 
            match List.distinctBy (function Value v -> doCheck v | _ ->  Unknown) ops with 
            | [] -> List Empty
            | [Value(gp)]  -> doCheck gp |> List
            | _ -> Mixed
        | Tuple(a, b) -> Scalar (sprintf "Scalar<%s,%s>" (doCheck a |> toString) ((doCheck b |> toString)))
        | Dist(vs) -> 
            match List.distinctBy (fst >> doCheck) vs with 
            | [] -> Distr Empty
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
    //3 + 3 = Scalara + Scalara = Scalara
    //3 + NoValue = T + Unknown = T
    //NoValue + 3 = Unknown + T = T
    let ``Test Addition`` (TwoSimilarTypes (value1,value2)) = 
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = [Value value1;Value value2] |> opList |> call Total >>= "result" |> es "result" |> toTyped
        match value1Type,value2Type,result with 
        | Scalar a, Scalar a', Scalar a''  -> Expect.allEqual [a; a'; a'' ] a'' "Scalara + Scalara = Scalara"
        | Unknown,         a',        a''  -> Expect.allEqual [a';a'' ] a''     "NoValue + Scalara = Scalara"
        |        a, Unknown  ,        a''  -> Expect.allEqual [a; a'' ] a''     "Scalara + NoValue = Scalara"
        | Distr Empty,     _ , Distr Empty
        |      _ ,Distr Empty, Distr Empty -> ()
        | Distr a,   Distr a', Distr a''   -> Expect.allEqual [a; a'; a'' ] a'' "Distra + Distra = Distra"
        | Distr a,  (Scalar _ as a'), Distr a''   -> Expect.allEqual [a; a'; a'' ] a'' "Distra + Scalara = Distra"
        | (Scalar _ as a), Distr a', Distr a''    -> Expect.allEqual [a; a'; a'' ] a'' "Distra + Scalara = Distra"
        | a,b,c -> failwith <| sprintf "Values don't all have the correct dimension %A+%A=%A?" a b c
    //Double D6 = Scalara x Scalara Dist = Scalara Dist
    //3 × 3 = Scalara × Scalara = Scalara
    let ``Test Multiplication`` (TwoSimilarTypes (value1,value2)) = 
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = [Value value1;Value value2] |> opList |> call Product >>= "result" |> es "result" |> toTyped
        match value1Type,value2Type,result with 
        | Scalar a,         Distr(Scalar a'), Distr(Scalar a'') 
        | Distr(Scalar a'), Scalar a ,        Distr(Scalar a'') -> Expect.isTrue (a = a'' && a  = a' ) "Scalars not same type"
        | Scalar a,         Scalar a',        Scalar a''        -> Expect.isTrue (a = a'' && a' = a'') "Scalars not same type"
        | Unknown ,         _  ,              Unknown    
        | _ ,               Unknown  ,        Unknown    -> ()
        | a,b,c -> failwith <| sprintf "%A, %A, %A don't all have the same dimension" a b c        
         
    let ``3 x 3 = Repeat 3 3 = Scalara x Scalarb = Scalara list`` (ScalarType value1) (ScalarType value2) = 
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = repeatOp (Value value1) (Value value2) >>= "result" |> es "result" |> toTyped
        match value1Type,value2Type,result with 
        | Scalar a, _, List (Scalar a'') -> Expect.isTrue (a = a'') "Scalars not same type"
        | a,b,c -> failwith <| sprintf "%A, %A, %A don't all have the same dimension" a b c
    let ``3D6 = Repeat D6 3 = Scalara Dist x Scalarb  = Scalara Dist List`` (DistType value1) (ScalarType value2) = 
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = repeatOp (Value value1) (Value value2) >>= "result" |> es "result" |> toTyped
        match value1Type,value2Type,result with 
        | Distr(Scalar a), _, List (Distr(Scalar a'')) -> Expect.isTrue (a = a'') "Scalars not same type"
        | a,b,c -> failwith <| sprintf "%A, %A, %A don't all have the same dimension" a b c

    let ``D3D6s = Repeat D6 D3 = Scalara Dist x Scalarb Dist = Scalara Dist List Dist`` (DistType value1) (DistType value2) = 
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = repeatOp (Value value1) (Value value2) >>= "result" |> es "result" |> toTyped
        match value1Type,value2Type,result with 
        | Distr(Scalar a), Distr(Scalar _), Distr(List(Distr(Scalar a''))) -> Expect.isTrue (a = a'') "Scalars not same type"
        | a,b,c -> failwith <| sprintf "%A, %A, %A don't all have the same dimension" a b c
    let ``D6 3 = Repeat 3 D6 = Scalara x Scalarb Dist = Scalara Dist List`` (ScalarType value1) (DistType value2) = 
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = repeatOp (Value value1) (Value value2) >>= "result" |> es "result" |> toTyped
        match value1Type,value2Type,result with 
        | Distr(Scalar a), Distr(Scalar _), List(Distr(Scalar a'')) -> Expect.isTrue (a = a'') "Scalars not same type"
        | a,b,c -> failwith <| sprintf "%A, %A, %A don't all have the same dimension" a b c
    //let ``Repeat [4] 3 = List Scalara x Scalarb = List List Scalar a``  = 
    testList "Repeat Tests" [
        testPropertyWithConfig config "3 + 3" ``Test Addition``
        testPropertyWithConfig config "3 * 3, Double D6" ``Test Multiplication``
        testPropertyWithConfig config "3 x 3 = Repeat 3 3 = Scalara x Scalarb = Scalara list" ``3 x 3 = Repeat 3 3 = Scalara x Scalarb = Scalara list``
        testPropertyWithConfig config "3D6 = Repeat D6 3 = Scalara Dist x Scalarb  = Scalara Dist List" ``3D6 = Repeat D6 3 = Scalara Dist x Scalarb  = Scalara Dist List``
        testPropertyWithConfig config "D3D6s = Repeat D6 D3 = Scalara Dist x Scalarb Dist = Scalara Dist List Dist" ``D3D6s = Repeat D6 D3 = Scalara Dist x Scalarb Dist = Scalara Dist List Dist``
        testPropertyWithConfig config "D6 3 = Repeat 3 D6 = Scalara x Scalarb Dist = Scalara Dist List" ``D6 3 = Repeat 3 D6 = Scalara x Scalarb Dist = Scalara Dist List``
    ]
