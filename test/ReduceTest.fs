module ReduceTests

open GameActions.Primitives.Types
open GameActions.Primitives.State
open Expecto
open GameActions.GameActionsList.State

let (==?) actual expected = Expect.equal expected actual ""

[<Tests>]
let tests =
    let eval x op =
        get x
        |> op
        |> evalOp Map.empty
    
    let d6Dist =
        [ 1..6 ]
        |> List.map (Int)
        |> List.rev
        |> Distribution.uniformDistribution
        |> Dist
        |> Value
    
    let plusTest plus =
        let ws = dPlus 6 plus >>= "WS"
        
        let expectedWS =
            [ 1..6 ]
            |> List.map (fun i -> 
                   if i >= plus then Check(Check.Pass(Int 1)), 1.0
                   else Check(Check.Fail(Int 1)), 1.0)
            |> Distribution.countedCases
        testList (sprintf "%d+ Tests" plus) [ test "WS Test Std" { 
                                                  match ws |> eval "WS" with
                                                  | (Value(Dist(result))) -> Expect.equal result expectedWS ""
                                                  | x -> failtest <| sprintf "Result is wrong type %A" x
                                              } ]
    
    let psychicDiceTest =
        let psychicDice =
            [ d6; d6 ]
            |> opList
            >>= "PsychicDice"
        testList "Some Tests" [ test "Psychic Dice std" { 
                                    let result = psychicDice |> eval "PsychicDice"
                                    result ==? opList [ d6Dist; d6Dist ]
                                } ]
    
    let psychicTotalTest =
        let psychicTest =
            [ d6; d6 ]
            |> opList
            |> total
            >>= "Psychic Test"
        testList "Some Tests" [ test "Psychic Total std" { 
                                    let expected = Distribution.dist { let! d1 = Distribution.uniformDistribution [ 1..6 ]
                                                                       let! d2 = Distribution.uniformDistribution [ 1..6 ]
                                                                       return Int(d1 + d2) }
                                    match psychicTest |> eval "Psychic Test" with
                                    | Value(Dist(result)) -> Expect.containsAll result.Probabilities expected.Probabilities ""
                                    | x -> failtest <| sprintf "Result is wrong type %A" x
                                } ]
    
    testList "Reduce Tests" [ test "All evaluations of straight values return same value" { 
                                  let input = Value(Int(6))
                                  let variableName = "A"
                                  
                                  let results =
                                      input
                                      >>= variableName
                                      |> eval variableName
                                  Expect.equal results input "All should return straight values"
                              }
                              plusTest 1
                              plusTest 2
                              plusTest 3
                              plusTest 4
                              plusTest 5
                              plusTest 6
                              plusTest 7
                              psychicDiceTest
                              psychicTotalTest ]
