module ReduceTests
open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.Models.State
open Check
open Expecto

let (==?) actual expected = Expect.equal expected actual ""

[<Tests>]
let tests = 
    let es x op = get x |> op |> evalOp standardCall Map.empty<_,_> 
    let ea x op = get x |> op |> evalOp avgCall Map.empty<_,_> 
    let e x op = get x |> op |> evalOp sampleCall Map.empty<_,_> 
    let d6Dist =  [1..6] |> List.map (Int) |> List.rev |> Distribution.uniformDistribution |> Dist |> Value
    let plusTest plus = 
        let WS = dPlus D6 plus >>= "WS"
        let expectedWS = List.init 6 ((+) 1 >> fun i -> if i >= plus then Check(Pass(Int(i))) else Check(Fail(Int(i)))) |> Distribution.uniformDistribution 
        testList (sprintf "%d+ Tests" plus) [
            test "WS Test Std" {
                let (Value(Dist(result))) = WS |> es "WS"
                Expect.containsAll result expectedWS ""
            }
            test "WS Test avg" {
                let result = WS |> ea "WS"
                let wrap f = Value(Check(f(Float(3.5))))
                let expected = if 3.5 >= float plus then wrap Pass else wrap Fail
                result ==? expected
            }
            test "WS Test Sample" {
                let (Value result) = WS |> e "WS"
                Expect.contains (List.map fst expectedWS) result ""
            }
        ]    
    let psychicDiceTest = 
        let psychicDice = [d6;d6] |> opList >>= "PsychicDice"
        
        testList "Some Tests" [
            test "Psychic Dice std" {
                let result = psychicDice |> es "PsychicDice"
                result ==? ParamArray[d6Dist;d6Dist]                    
            }
            test "Psychic Dice avg" {
                let result = psychicDice |> ea "PsychicDice"
                result ==? ParamArray[Value(Float(3.5));Value(Float(3.5))]                    
            }
            test "Psychic Dice sample" {
                let (ParamArray[Value(a);Value(b)]) = psychicDice |> e "PsychicDice"
                let expected = [1..6] |> List.map (Int) 
                Expect.contains expected a ""
                Expect.contains expected b ""
            }
        ]
    let psychicTotalTest = 
        let psychicTest = [d6;d6] |> opList |> total >>= "PsychicTest"
        testList "Some Tests" [
            test "Psychic Dice std" {
                let expected = Distribution.dist {
                    let! d1 = Distribution.uniformDistribution [1..6]
                    let! d2 = Distribution.uniformDistribution [1..6]
                    return Int(d1 + d2)
                } 
                let (Value(Dist(result))) = psychicTest |> es "PsychicTest"
                Expect.containsAll result expected ""
            }
            test "Psychic Dice avg" {
                let result = psychicTest |> ea "PsychicTest"
                let expected = Value (Float 7.0)
                result ==? expected            
            }
            test "Psychic Dice sample" {
                let (Value a) = psychicTest |> e "PsychicTest"
                let expected = [2..12] |> List.map (Int) 
                Expect.contains expected a ""
            }
        ]    
    let unfoldVsStraight = 
        let passOrFailCount = [dPlus D6 3] |> opList |> call Count
        let times = 3
        let ``Three Passes`` = List.init times (fun _ -> passOrFailCount) |> opList |> call Total >>= "ThreePasses"
        let ``Three Passes Unfold`` = unfoldOp passOrFailCount (vInt times) |> call Total >>= "ThreePasses"
        test "Straight Array should be the same as unfold" {
            (es  "ThreePasses" ``Three Passes``) ==? (es "ThreePasses" ``Three Passes Unfold``)
        }
    testList "Reduce Tests" [
        test "All evaluations of straight values return same value" {
            let input = Value(Int(6))
            let variableName = "A"
            let results = 
                [ input >>= variableName |> es variableName
                  input >>= variableName |> ea variableName
                  input >>= variableName |> e variableName ]
            Expect.allEqual results input "All should return straight values"
        }
        plusTest 1
        plusTest 2
        plusTest 3
        plusTest 4
        plusTest 5
        plusTest 6
        plusTest 7
        psychicDiceTest
        psychicTotalTest
        unfoldVsStraight
    ]
