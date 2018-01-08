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
type GamePrimitiveType = Scalar of string | List of GamePrimitiveType | Pass of GamePrimitiveType | Fail of GamePrimitiveType | Distr of GamePrimitiveType | Empty | Mixed | Unknown
let rec toString = function
    | Fail a -> sprintf "Check<%s>" <| toString a
    | Pass a -> sprintf "Check<%s>" <| toString a
    | Scalar s -> sprintf "Scalar<%s>" s
    | List gpt -> sprintf "List<%s>" <| toString gpt
    | Distr gpt -> sprintf "Dist<%s>" <| toString gpt
    | Mixed -> "Mixed"
    | Unknown -> "Unknown"
    | Empty -> "Empty"

let toTyped op =
    let rec (|IsEmpty|_|) = function 
        | Empty -> Some Empty
        | List(IsEmpty(e)) 
        | Distr(IsEmpty(e)) 
        | Pass(IsEmpty(e)) 
        | Fail(IsEmpty(e)) -> Some e
        | _ -> None 
    let rec doCheck = function
        | Int _                             -> Scalar "Int"
        | Str(_)                            -> Scalar "Str"
        //| Float(_)                          -> Scalar "Float"
        | Check(Check.Fail(gp))             -> Fail (doCheck gp)
        | Check(Check.Pass(gp))             -> Pass (doCheck gp)
        | NoValue                           -> Unknown
        | ParamArray(ops)  ->
            ops
            |> List.fold (fun acc elem ->
                match elem with
                | Value elem ->
                    match acc,doCheck elem with
                    | IsEmpty(_), a -> a
                    | a, IsEmpty(_) -> a
                    | (a, b) when a <> b -> Mixed
                    | (a,_) -> a
                | _ -> Unknown ) Empty
            |> List
        | Tuple(a, b) -> Scalar (sprintf "Scalar<%s,%s>" (doCheck a |> toString) ((doCheck b |> toString)))
        | Dist(vs) ->
            vs
            |> List.fold (fun acc (elem,_) ->
                match acc,doCheck elem with
                  | IsEmpty(_), a -> a
                  | a, IsEmpty(_) -> a
                  | (a, b) when a <> b -> Mixed
                  | (a,_) -> a ) Empty
            |> Distr
    match op with
    | Value(gp) -> doCheck gp
    | _ -> Unknown
// [(ParamArray [], 4.940656458e-324);
//       (ParamArray [Value (Float 0.0)], 4.940656458e-324);
//       (ParamArray [], 0.07142857143); (ParamArray [], 0.5862068966)] 
//             |> List.scan (fun acc (elem,_) ->
//                 match acc,doCheck elem with
//                   | IsEmpty c, a -> printfn "A: %A" c;a
//                   | a, IsEmpty c -> printfn "B: %A" c;a
//                   | (a, b) when a <> b -> Mixed
//                   | (a,_) -> printfn "C: %A" acc; a ) Empty   
// [(ParamArray [], 4.940656458e-324); (ParamArray [], 4.940656458e-324);
//   (ParamArray [Value (Int 0); Value (Int 0)], 4.940656458e-324);
//   (ParamArray [], 4.940656458e-324)]  |> Dist |> Value |> toTyped
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
//let ea x op = get x |> op |> evalOp avgCall Map.empty<_,_>
let e x op = get x |> op |> evalOp sampleCall Map.empty<_,_>

[<Tests>]
let tests =
    //3 + 3 = Scalara + Scalara = Scalara
    //3 + NoValue = T + Unknown = T
    //NoValue + 3 = Unknown + T = T
    let ``Test Addition`` (TwoSimilarTypes (value1,value2)) =
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = [Value value1;Value value2] |> opList |> call Total >>= "result" |> es "result" 
        let resultType = result |> toTyped
        let rec checkTypes (value1Type,value2Type,resultType) =
            match value1Type,value2Type,resultType with
            //No change to type            
            | Unknown,         a',        a''  -> Expect.allEqual [ a'; a'' ] a'' "NoValue + A = A"
            |        a,   Unknown,        a''  -> Expect.allEqual [ a ; a'' ] a'' "A + NoValue = A"
            |    Empty,        a',        a''  -> Expect.allEqual [ a'; a'' ] a'' "Empty + X = X"
            |        a,     Empty,        a''  -> Expect.allEqual [ a ; a'' ] a'' "Empty + X = X"
            //Cartesian product goes to empty 
            | Distr Empty,     _ , Distr Empty
            |      _ ,Distr Empty, Distr Empty -> ()
            //Autolift. Fails are ignored, things are automatically made passing
            |   Fail _,   Pass a',   Pass a''   
            |   Fail _,        a',   Pass a''  -> checkTypes (a', a', a'')  
            |   Pass a,   Fail _ ,   Pass a''                  
            |        a,   Fail _ ,   Pass a''  -> checkTypes (a , a , a'')  
            |   Pass a,   Pass a',   Pass a''   
            |   Pass a,        a',   Pass a''    
            |        a,   Pass a',   Pass a''   
            |   Fail a,   Fail a',   Fail a''   
            |  Distr a,  Distr a',  Distr a''  
            |  Distr a,        a',  Distr a''  
            |        a,  Distr a',  Distr a''    
            //Defer check
            | List a,     List a',   List a''  -> checkTypes (a, a', a'')
            | Scalar a, Scalar a', Scalar a''  -> Expect.allEqual [a; a'; a'' ] a'' "Scalara + Scalara = Scalara"
            |        a,         b,          c  -> failtest <| sprintf "Values don't all have the correct dimension %A+%A=%A?\nValue: %A" a b c result
        checkTypes (value1Type,value2Type,resultType)
    //Double D6 = Scalara x Scalara Dist = Scalara Dist
    //3 × 3 = Scalara × Scalara = Scalara
    let ``Test Multiplication`` (TwoSimilarScalarTypes (value1,value2)) =
        let value1Type = value1 |> Value |> toTyped
        let value2Type = value2 |> Value |> toTyped
        let result = [Value value1;Value value2] |> opList |> call Product >>= "result" |> es "result" 
        let resultType = result |> toTyped
        let rec checkTypes (value1Type,value2Type,resultType) =
            match value1Type,value2Type,resultType with
            //Multiply by 0 -> 0       
            | Unknown,          _,    Unknown  -> ()
            |        _,   Unknown,    Unknown  -> ()
            |    Empty,         _,      Empty  -> ()
            |        _,     Empty,      Empty  -> ()
            //Cartesian product goes to empty 
            | Distr Empty,     _ , Distr Empty
            |      _ ,Distr Empty, Distr Empty -> ()
            //Autolift. Fails are ignored, things are automatically made passing
            |  Distr a,  Distr a',  Distr a''  
            |  Distr a,        a',  Distr a''  
            |        a,  Distr a',  Distr a''    
            |   Pass a,   Pass a',   Pass a''   
            |   Fail a,   Fail a',   Fail a''   
            |   Pass a,        a',   Pass a''    
            |        a,   Pass a',   Pass a''  -> checkTypes (a , a', a'') 
            |   Fail _,   Pass a',   Fail a''    
            |   Fail _,        a',   Fail a''  -> checkTypes (a', a', a'')  
            |   Pass a,   Fail _ ,   Fail a''  // Pass * Fail = Fail, like -1          
            |        a,   Fail _ ,   Fail a''  -> checkTypes (a , a , a'')   
            //Defer check
            | Scalar a, Scalar a', Scalar a''  -> Expect.allEqual [a; a'; a'' ] a'' "Scalara + Scalara = Scalara"
            |        a,         b,          c  -> failtest <| sprintf "Values don't all have the correct dimension %A+%A=%A?\nValue: %A" a b c result
        checkTypes (value1Type,value2Type,resultType)

    let ``Repeat Tests`` (value1:ListDistScalarType) (value2:ListDistScalarType) =
        let (value1,value2) =DistGamePrimitive (Dist [(Int 3, 4.940656458e-324)]), DistGamePrimitive (Dist [(Int 0, 4.940656458e-324)])
        let value1' = value1.ToGamePrimitive() 
        let value2' = value2.ToGamePrimitive() 
        let value1Type = value1' |> Value |> toTyped
        let value2Type = value2' |> Value |> toTyped
        let result = repeatOp (Value value1') (Value value2') >>= "result" |> es "result"
        let resultType = result |> toTyped
        let rec checkTypes (value1Type,value2,value2Type,resultType) =
            match value1Type,value2Type,resultType with
            | _,        Distr Empty,     Distr Empty        -> ()
            | _,        List Empty,      List Empty         -> ()
            | _,        Pass Empty,      Pass Empty         -> ()
            | _,        Empty,           Empty              -> ()
            | _,        Distr Unknown,   Distr Unknown      -> ()
            | _,        List Unknown,    List Unknown       -> ()
            | _,        Pass Unknown,    Pass Unknown       -> ()
            | _,        Unknown,         Unknown            -> ()
            | _,        Fail _,          Unknown            -> ()
            | _,        Scalar "Int",    List Empty         when value2 = Int 0 || value2 = Check (Check.Pass (Int 0)) 
                                                            -> ()
            // | Pass a,   Pass(a'),        a''         
            | a,        Pass(a'),        a''                -> checkTypes (a, value2, a', a'')
            // | Pass a,   a',              a''                
            //Adds a distr to the outside
            | _,       Distr(Scalar "Int"), Distr(List Empty) ->
                let (Dist value2') = value2
                if List.forall(fun (a,b) -> a = Int 0 || a = Check (Check.Pass (Int 0)) ) value2' then () 
                else failtest "Dist is empty, but scalar is not 0"
            | a,        Distr a',        Distr(a'')         -> checkTypes (a, value2,a', a'') //"D3D6s\nD6 x 3\n[A;b;c] x D6  = \nRepeat D6 D3\nRepeat 3 D6\nRepeat D6 [a;b;c] = \nA x B Scalar Dist = A List Dist"
            //Adds a List to the outside
            | a,        List  a',        List (a'')         -> checkTypes (a, value2,a', a'')
            | a,        Scalar _,        List (a'')         ->  Expect.equal a a'' "3 x 3\n3D6\n[A;b;c] x 3 = \nRepeat 3 3\nRepeat D6 3\nRepeat 3 [a;b;c] =\nA x B = A list"
            |        a,         b,          c  -> failtest <| sprintf "Values don't all have the correct dimension %A+%A=%A?\nValue: %A" a b c result
        checkTypes (value1Type,value2',value2Type,resultType)        
    //let ``Repeat [4] 3 = List Scalara x Scalarb = List List Scalar a``  =
    testList "Repeat Tests" [
        testPropertyWithConfig config "3 + 3" ``Test Addition``
        testPropertyWithConfig config "3 * 3, Double D6" ``Test Multiplication``
        testPropertyWithConfig config "3 x D3 = Repeat 3 D3, D3 x 3, D3 x D6, 3 x 3" ``Repeat Tests``
    ]
