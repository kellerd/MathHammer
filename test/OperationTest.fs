module OperationTests
open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.Models.State
open Distribution
open ExpectoFsCheck
let (==?) x y = Expect.equal x y ""

[<Tests>]
let tests = 
    testList "Operation Tests" [
        let stdEval = normalizeOp >> evalOp standardCall Map.empty<_,_>
        yield test "Evalled D6 equal std distribution of integers, reversed" {
            let result = stdEval d6
            let expected = [1..6] |> List.map (Int) |> List.rev |> uniformDistribution |> Dist |> Value
            result ==? expected
        }
        
        yield test "Evalled D3 equal std distribution of integers, reversed" {
            let result = stdEval d3
            let expected = [1..3] |> List.map (Int) |> List.rev |> uniformDistribution |> Dist |> Value
            result ==? expected
        }
        //Let x = 3 returns 3 as well as binding to environment
        let retValueIsSame f v = 
            let evaled = Let("x", v ,Var ("x")) |> f |> evalOp standardCall Map.empty<_,_> 
            let evaled' = v |> f |> evalOp standardCall Map.empty<_,_> 
            evaled ==? evaled'
        yield ftestProperty (1865288075, 296281834) "let x = 3 returns 3 evaluated without normalization" (retValueIsSame id)
        yield ftestProperty (1865288075, 296281834) "let x = 3 returns 3 evaluated with normalization" (retValueIsSame normalizeOp)
        //Let x = some number in
        //x + some other number
        let addition x y  =    
            let (Value(Dist(d))) =
                Let("x", Value(y) ,App(Call Total, ParamArray([Value(x);Var ("x")])))
                |> evalOp standardCall Map.empty<_,_>
            let expected = always (x + y)
            d ==? expected
            
        yield ftestProperty (1865288075, 296281834) "Addition in child scope is valid" addition


        let totalOfXIsX x = 
            let v = Value(x)
            let (Value(Dist(d))) =
                Let("x", v ,App(Call Total, v)) 
                |> evalOp standardCall Map.empty<_,_>
            let expected = always (x)
            d ==? expected
        yield ftestProperty (1865288075, 296281834)  "Total of x is x" totalOfXIsX 
        let countOfOneXIsOneX x = 
            let v = Value(Check(Check.Pass(x)))
            let (Value(Dist(d))) =
                Let("x", v ,App(Call Count, v)) 
                |> evalOp standardCall Map.empty<_,_>
            let expected = always (Check(Check.Tuple (Int(1),Int(0))))
            d ==? expected
        yield ftestProperty (1865288075, 296281834)  "Count of one passed result is 1"  countOfOneXIsOneX 

        let unfoldD6 x = 
            let ws = dPlus D6 3
            let a = vInt x
            let unfold = unfoldOp ws a
            let (ParamArray(result))= unfold |> evalOp standardCall Map.empty<_,_>
            Expect.equal (List.length result) x "Length of unfold should be same as length input"
            match result with 
            | [] -> ()
            | head::tail -> Expect.allEqual tail head "All elements in unfold should be the same"
        yield ftestProperty (1865288075, 296281834)  "Unfold D6 by 3" unfoldD6 
    ]