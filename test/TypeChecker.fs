module TypeCheckerTest

open FsCheckGen
open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open GameActions.Primitives.TypeChecker

let (==?) x y =
    //printfn "%A" x
    //printfn "%A" y
    Expect.equal x y ""

//For ease of use, there should be only one Check value, and it should fall down to the leaves of the tree.
let floor gpType =
    let rec aux isPass acc =
        let f =
            match isPass with
            | Some true -> Pass
            | Some false -> Fail
            | None -> id
        match acc with
        | Pair(p, p2) -> Pair(aux isPass p, aux isPass p2)
        | Pass gp -> aux (Option.orElse (Some true) isPass) gp
        | Fail gp -> aux (Some false) gp
        | Distr p -> Distr(aux isPass p)
        | Empty -> acc
        | Mixed -> acc
        | Unknown -> acc
        | Scalar _ -> f acc
        | List p -> List(aux isPass p)
    aux None gpType

let eval x op =
    get x
    |> op
    |> evalOp Map.empty

[<Tests>]
let tests =
    //3 + 3 = Scalara + Scalara = Scalara
    //3 + NoValue = T + Unknown = T
    //NoValue + 3 = Unknown + T = T
    let ``Test Addition`` (TwoSimilarTypes(value1, value2)) =
        //let (value1,value2) = (Check (Check.Fail (ParamArray [Value (Int 1); Value (Int 0); Value (Int 2); Value (Int 2)])),ParamArray [])
        let value1Type =
            value1
            |> Value
            |> toTyped
        
        let value2Type =
            value2
            |> Value
            |> toTyped
        
        let result =
            [ Value value1
              Value value2 ]
            |> opList
            |> call Total
            >>= "result"
            |> eval "result"
        
        let resultType = result |> toTyped
        
        let rec checkTypes (value1Type, value2Type, resultType) =
            match value1Type, value2Type, resultType with
            //No change to type            
            | Unknown, a', a'' -> Expect.allEqual [ a'; a'' ] a'' "NoValue + A = A"
            | a, Unknown, a'' -> Expect.allEqual [ a; a'' ] a'' "A + NoValue = A"
            | Empty, a', a'' -> Expect.allEqual [ a'; a'' ] a'' "Empty + X = X"
            | a, Empty, a'' -> Expect.allEqual [ a; a'' ] a'' "Empty + X = X"
            //Cartesian product goes to empty 
            | Distr Empty, _, Distr Empty | _, Distr Empty, Distr Empty -> ()
            //Autolift. Fails are ignored, things are automatically made passing
            | IsEmpty _, Fail a, Pair(Pass _, Fail a'') | Fail a, IsEmpty _, Pair(Pass _, Fail a'') -> checkTypes (a, a, a'')
            | Fail a, Pass a', Pair(Pass a'', Fail _) | Fail a, a', Pair(Pass a'', Fail _) -> checkTypes (a, a', a'')
            | Pass a, Fail _, Pair(Pass a'', Fail _) | a, Fail _, Pair(Pass a'', Fail _) -> checkTypes (a, a, a'')
            | Pass a, Pass a', Pass a'' | Pass a, a', Pass a'' | a, Pass a', Pass a'' | Fail a, Fail a', Fail a'' | Distr a, Distr a', Distr a'' | Distr a, 
                                                                                                                                                   a', 
                                                                                                                                                   Distr a'' | a, 
                                                                                                                                                               Distr a', 
                                                                                                                                                               Distr a'' //Defer check
                                                                                                                                                                         | List(Fail a), 
                                                                                                                                                                           List(a'), 
                                                                                                                                                                           Pair(List(Pass a''), 
                                                                                                                                                                                List(Fail _)) | List a, 
                                                                                                                                                                                                List(Fail a'), 
                                                                                                                                                                                                Pair(List(Pass a''), 
                                                                                                                                                                                                     List(Fail _)) | List a, 
                                                                                                                                                                                                                     List a', 
                                                                                                                                                                                                                     List a'' -> 
                checkTypes (floor a, floor a', floor a'')
            | Scalar a, Scalar a', Scalar a'' -> Expect.allEqual [ a; a'; a'' ] a'' "Scalara + Scalara = Scalara"
            | a, b, c -> failtest <| sprintf "Values don't all have the correct dimension %A+%A=%A?\nValue: %A" a b c result
        checkTypes (value1Type, value2Type, resultType)
    
    //Double D6 = Scalara x Scalara Dist = Scalara Dist
    //3 × 3 = Scalara × Scalara = Scalara
    let ``Test Multiplication`` (TwoSimilarScalarTypes(value1, value2)) =
        let value1Type =
            value1
            |> Value
            |> toTyped
        
        let value2Type =
            value2
            |> Value
            |> toTyped
        
        let result =
            [ Value value1
              Value value2 ]
            |> opList
            |> call Product
            >>= "result"
            |> eval "result"
        
        let resultType = result |> toTyped
        
        let rec checkTypes (value1Type, value2Type, resultType) =
            match value1Type, value2Type, resultType with
            //Multiply by 0 -> 0       
            | Unknown, _, Unknown -> ()
            | _, Unknown, Unknown -> ()
            | Empty, _, Empty -> ()
            | _, Empty, Empty -> ()
            //Cartesian product goes to empty 
            | Distr Empty, _, Distr Empty | _, Distr Empty, Distr Empty -> ()
            //Autolift. Fails are ignored, things are automatically made passing
            | Distr a, Distr a', Distr a'' | Distr a, a', Distr a'' | a, Distr a', Distr a'' | Pass a, Pass a', Pass a'' | Fail a, Fail a', 
                                                                                                                           Fail a'' | Pass a, a', 
                                                                                                                                      Pass a'' | a, 
                                                                                                                                                 Pass a', 
                                                                                                                                                 Pass a'' -> 
                checkTypes (a, a', a'')
            | Fail _, Pass a', Fail a'' | Fail _, a', Fail a'' -> checkTypes (a', a', a'')
            | Pass a, Fail _, Fail a'' // Pass * Fail = Fail, like -1          
                                       | a, Fail _, Fail a'' -> checkTypes (a, a, a'')
            //Defer check
            | Scalar a, Scalar a', Scalar a'' -> Expect.allEqual [ a; a'; a'' ] a'' "Scalara * Scalara = Scalara"
            | a, b, c -> failtest <| sprintf "Values don't all have the correct dimension %A*%A=%A?\nValue: %A" a b c result
        checkTypes (value1Type, value2Type, resultType)
    
    let ``Repeat Tests`` (value1 : ListDistScalarType) (value2 : ListDistScalarType) =
        let value1' = value1.ToGamePrimitive()
        let value2' = value2.ToGamePrimitive()
        
        let value1Type =
            value1'
            |> Value
            |> toTyped
        
        let value2Type =
            value2'
            |> Value
            |> toTyped
        
        let result =
            repeatOp (Value value1') (Value value2')
            >>= "result"
            |> eval "result"
        
        let resultType = result |> toTyped
        
        let rec checkTypes (value1Type, value2, value2Type, resultType) =
            match value1Type, value2Type, resultType with
            | _, Distr Empty, Unknown -> ()
            | _, List Empty, Unknown -> ()
            | _, Pass Empty, Pass Empty -> ()
            | _, Empty, Empty -> ()
            | _, Distr Unknown, Distr Unknown -> ()
            | _, List Unknown, List Unknown -> ()
            | _, Pass Unknown, Pass Unknown -> ()
            | _, Unknown, List Empty -> ()
            | _, Fail(Scalar "Int"), List Empty | _, Pass(Scalar "Int"), List Empty | _, Scalar "Int", List Empty when value2 = Int 0 
                                                                                                                       || value2 = Check
                                                                                                                                       (Check.Pass
                                                                                                                                            (Int 
                                                                                                                                                 0)) 
                                                                                                                       || value2 = Check
                                                                                                                                       (Check.Fail
                                                                                                                                            (Int 
                                                                                                                                                 0)) -> 
                ()
            //Adds a distr to the outside
            | _, Distr(Scalar "Int"), Distr(List Empty) -> 
                match value2 with
                | Dist value2' -> 
                    if List.forall (fun (a, _) -> a = Int 0 || a = Check(Check.Pass(Int 0))) value2'.Probabilities then ()
                    else failtest "Dist is empty, but scalar is not 0"
                | _ -> failtest "Value2 expected to be a distribution"
            | Distr Empty, Distr _, Distr(List(Distr Empty)) -> ()
            //| Distr a,  Distr a',        Distr(a'')         -> checkTypes (a, value2,a', a'')                      
            | a, Distr a', Distr(a'') -> checkTypes (a, value2, a', a'') //"D3D6s\nD6 x 3\n[A;b;c] x D6  = \nRepeat D6 D3\nRepeat 3 D6\nRepeat D6 [a;b;c] = \nA x B Scalar Dist = A List Dist"
            //Adds a List to the outside
            | a, List a', List(a'') -> checkTypes (a, value2, a', a'')
            | a, Fail(Scalar _), List(a'') -> 
                Expect.equal (floor (Fail a)) (floor a'') 
                <| sprintf "3 x 3\n3D6\n[A;b;c] x 3 = \nRepeat 3 3\nRepeat D6 3\nRepeat 3 [a;b;c] =\nA x B = A list %Ax%A=%A" value1' value2' 
                       result
            | a, Pass(Scalar _), List(a'') -> 
                Expect.equal (floor (Pass a)) (floor a'') 
                <| sprintf "3 x 3\n3D6\n[A;b;c] x 3 = \nRepeat 3 3\nRepeat D6 3\nRepeat 3 [a;b;c] =\nA x B = A list %Ax%A=%A" value1' value2' 
                       result
            | a, Scalar _, List(a'') -> 
                Expect.equal (floor a) (floor a'') 
                <| sprintf "3 x 3\n3D6\n[A;b;c] x 3 = \nRepeat 3 3\nRepeat D6 3\nRepeat 3 [a;b;c] =\nA x B = A list %Ax%A=%A" value1' value2' 
                       result
            | a, b, c -> failtest <| sprintf "Values don't all have the correct dimension %Ax%A=%A?\nValue: %A" a b c result
        checkTypes (value1Type, value2', value2Type, resultType)
    
    //let ``Repeat [4] 3 = List Scalara x Scalarb = List List Scalar a``  =
    testList "Repeat Tests" [ testPropertyWithConfig config "3 + 3" ``Test Addition``
                              testPropertyWithConfig config "3 * 3, Double D6" ``Test Multiplication``
                              testPropertyWithConfig config "3 x D3 = Repeat 3 D3, D3 x 3, D3 x D6, 3 x 3" ``Repeat Tests`` ]
