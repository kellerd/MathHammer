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