module OperationTests
open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.Models.State
open FsCheckGen
let (==?) x y = Expect.equal x y ""



[<Tests>]
let tests = 
    testList "Operation Tests" [
        let stdEval = normalizeOp >> evalOp standardCall Map.empty<_,_>
        yield test "Evalled D6 equal std distribution of integers, reversed" {
            let result = stdEval d6
            let expected = [1..6] |> List.map (Int) |> List.rev |> Distribution.uniformDistribution |> Dist |> Value
            result ==? expected
        }
        
        yield test "Evalled D3 equal std distribution of integers, reversed" {
            let result = stdEval d3
            let expected = [1..3] |> List.map (Int) |> List.rev |> Distribution.uniformDistribution |> Dist |> Value
            result ==? expected
        }
        //Let x = 3 returns 3 as well as binding to environment
        let retValueIsSame f v = 
            let evaled = Let("x", Value(v) ,Var ("x")) |> f |> evalOp standardCall Map.empty<_,_> 
            let evaled' = Value(v) |> f |> evalOp standardCall Map.empty<_,_> 
            evaled ==? evaled'
        yield testPropertyWithConfig config "let x = 3 returns 3 evaluated without normalization" (retValueIsSame id)
        yield testPropertyWithConfig config "let x = 3 returns 3 evaluated with normalization" (retValueIsSame normalizeOp)
        //Let x = some number in
        //x + some other number
        let addition x y  =   
            let result = 
                try 
                    Let("x", Value(y) ,App(Call Total, ParamArray([Value(x);Var ("x")])))
                    |> evalOp standardCall Map.empty<_,_>
                    |> Choice1Of2
                with ex -> Choice2Of2 (ex.Message.Substring(0,30))
            let expected = 
                try 
                    Value(x + y) |> Choice1Of2
                with ex -> Choice2Of2 (ex.Message.Substring(0,30))
            result ==? expected 
        yield testPropertyWithConfig config "Addition in child scope is valid" addition
        let totalOfXIsX x = 
            let expected = x |> Value
            let result =
                Let("x", expected ,App(Call Total, Var("x"))) 
                |> evalOp standardCall Map.empty<_,_>
            result ==? expected
        yield testPropertyWithConfig config "Total of x is x" totalOfXIsX
        let countOfOneXIsOneX x = 
            let v = Value(Check(Check.Pass(x)))
            let result =
                Let("x", v ,App(Call Count, v)) 
                |> evalOp standardCall Map.empty<_,_>
            let expected = Value(Dist(Distribution.always (Check(Check.Tuple (Int(1),Int(0))))))
            result ==? expected
        yield testPropertyWithConfig config "Count of one passed result is 1"  countOfOneXIsOneX 
    ]