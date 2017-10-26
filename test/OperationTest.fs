module OperationTests
open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.Models.State

open Distribution
[<Tests>]
let tests = 
    let stdEval = normalizeOp >> evalOp standardCall Map.empty<_,_>
    testList "Operation Standard" [
        
        testCase "Evalled D6 equal std distribution of integers, reversed" <| fun _ -> 
            let result = stdEval d6
            let expected = [1..6] |> List.map (Int) |> List.rev |> uniformDistribution |> Dist |> Value
            Expect.equal result expected <| sprintf "Evaluating a D6 should be [1..6] Ints as an even distribution" 

        
        testCase "Evalled D3 equal std distribution of integers, reversed" <| fun _ -> 
            let result = stdEval d3
            let expected = [1..3] |> List.map (Int) |> List.rev |> uniformDistribution |> Dist |> Value
            Expect.equal result expected <| sprintf "Evaluating a D6 should be [1..3] Ints as an even distribution"
       
    ]



// let v = Value(Int(3))
// //Let x = 3 returns 3 as well as binding to environment
// let retValueIsSame v f =
//     let evaled = Let("x", v ,Var ("x")) |> f |> evalOp standardCall Map.empty<_,_> 
//     let evaled' = v |> f |> evalOp standardCall Map.empty<_,_> 
//     evaled = evaled'
// retValueIsSame v id
// retValueIsSame v normalizeOp
// //Let x = some number in
// //x + some other number
// let addition x y  =    
//     let v = Value(Int(x))
//     let (Value(Dist(d))) =
//         Let("x", v ,App(Call Total, ParamArray([Value(Int(y));Var ("x")])))
//         |> evalOp standardCall Map.empty<_,_>
//     let expected = Distribution.always (Int(x + y))
//     printfn "Is %A = %A" d expected
//     d = expected
// //Let x = 6
// //Total of x is 6
// let totalOfXIsX x = 
//     let v = Value(Int(x))
//     let (Value(Dist(d))) =
//         Let("x", v ,App(Call Total, v)) 
//         |> evalOp standardCall Map.empty<_,_>
//     let expected = Distribution.always (Int(x))
//     printfn "Is %A = %A" d expected
//     d = expected
// //Count of one passed result is 1
// let countOfOneXIsOneX x = 
//     let v = Value(Check(Check.Pass(Int(x))))
//     let (Value(Dist(d))) =
//         Let("x", v ,App(Call Count, v)) 
//         |> evalOp standardCall Map.empty<_,_>
//     let expected = Distribution.always (Check(Check.Tuple (Int(1),Int(0))))
//     printfn "Is %A = %A" d expected
//     d = expected

// totalOfXIsX 6  
// addition 3 9    
// countOfOneXIsOneX 6  

// let unfoldD6 = 
//     let WS = dPlus D6 3
//     let A = vInt 3
//     let unfold = unfoldOp WS A
//     unfold 
//     |> evalOp standardCall Map.empty<_,_>