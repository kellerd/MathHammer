module OperationTests
open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.Models.State

open Distribution
[<Tests>]
let (==?) x y = Expect.equal x y ""
let tests = 
    let stdEval = normalizeOp >> evalOp standardCall Map.empty<_,_>
    testList "Operation Standard" [
        
        testCase "Evalled D6 equal std distribution of integers, reversed" <| fun _ -> 
            let result = stdEval d6
            let expected = [1..6] |> List.map (Int) |> List.rev |> uniformDistribution |> Dist |> Value
            result ==? expected

        
        testCase "Evalled D3 equal std distribution of integers, reversed" <| fun _ -> 
            let result = stdEval d3
            let expected = [1..3] |> List.map (Int) |> List.rev |> uniformDistribution |> Dist |> Value
            result ==? expected
       
    ]

[<Tests>]
let ``Operation Tests`` = 
    testList "Operation Tests" [
        let v = Value(Int(3))
        //Let x = 3 returns 3 as well as binding to environment
        let retValueIsSame v f =
            let evaled = Let("x", v ,Var ("x")) |> f |> evalOp standardCall Map.empty<_,_> 
            let evaled' = v |> f |> evalOp standardCall Map.empty<_,_> 
            evaled ==? evaled'
        yield test "let x = 3 returns 3 evaluated without normalization" { retValueIsSame v id }
        yield test "let x = 3 returns 3 evaluated with normalization" { retValueIsSame v normalizeOp }
        //Let x = some number in
        //x + some other number
        let addition x y  =    
            let v = Value(Int(x))
            let (Value(Dist(d))) =
                Let("x", v ,App(Call Total, ParamArray([Value(Int(y));Var ("x")])))
                |> evalOp standardCall Map.empty<_,_>
            let expected = always (Int(x + y))
            printfn "Is %A = %A" d expected
            d ==? expected
            
        yield test "Add 3 + 9" {addition 3 9}
        let totalOfXIsX x = 
            let v = Value(Int(x))
            let (Value(Dist(d))) =
                Let("x", v ,App(Call Total, v)) 
                |> evalOp standardCall Map.empty<_,_>
            let expected = always (Int(x))
            printfn "Is %A = %A" d expected
            d ==? expected
        yield test "Total of x is x" {totalOfXIsX 4}
        let countOfOneXIsOneX x = 
            let v = Value(Check(Check.Pass(Int(x))))
            let (Value(Dist(d))) =
                Let("x", v ,App(Call Count, v)) 
                |> evalOp standardCall Map.empty<_,_>
            let expected = always (Check(Check.Tuple (Int(1),Int(0))))
            printfn "Is %A = %A" d expected
            d ==? expected
        yield test "Count of one passed result is 1"  {countOfOneXIsOneX 6}  
        let unfoldD6 x = 
            let ws = dPlus D6 3
            let a = vInt x
            let unfold = unfoldOp ws a
            let (ParamArray(result))= unfold |> evalOp standardCall Map.empty<_,_>
            Expect.equal (List.length result) x "Length of unfold should be same as length input"
            match result with 
            | [] -> ()
            | head::tail -> Expect.allEqual tail head "All elements in unfold should be the same"
        yield test "Unfold D6 by 3" {unfoldD6 3} 
    ]