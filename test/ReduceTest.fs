open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.Models.State
open Expecto
[<Tests>]
let tests = 
    let es x op = get x |> op |> evalOp standardCall Map.empty<_,_> 
    let ea x op = get x |> op |> evalOp avgCall Map.empty<_,_> 
    let e x op = get x |> op |> evalOp sampleCall Map.empty<_,_> 

    testList "Reduce Tests" [
        test "All evaluations of straight values return same value" <| fun _
            let input = Value(Int(6))
            let variableName = "A"
            let results = 
                [ input >>= variableName |> es variableName
                  input >>= variableName |> ea variableName
                  input >>= variableName |> e variableName ]
            Expect.allEqual results input "All should return straight values"
        let plus = 3
        let WS = dPlus D6 plus >>= "WS"
        let expectedWS = List.init 6 (fun i -> if i >= plus then Pass i else Fail i) |> toUniformDistribution
        testList "D+ Tests" [
            test "WS Test Std" <| fun
                let result = WS |> es "WS"
                Expect.equal results expectedWs
        ]    
    ]

WS |> es "WS"
WS |> ea "WS"
WS |> e "WS"
let psychicDice = [d6;d6] |> opList >>= "PsychicDice"
psychicDice |> es "PsychicDice"
psychicDice |> ea "PsychicDice"
psychicDice |> e "PsychicDice"
let psychicTest = [d6;d6] |> opList |> total >>= "PsychicTest"
psychicTest |> es "PsychicTest"
psychicTest |> ea "PsychicTest"
psychicTest |> e "PsychicTest"

open Determinism
open MathHammer.Types

<@ let x = 5
   x @>

let x = dPlus D6 3 >>= "x" <| get "x"
let a = vInt 3 >>= "a"  <| get "a"
let env = Map.empty<_,_>
let r = evalOp standardCall Map.empty<_,_> x
let r2 = evalOp standardCall env a

let passes = lam "x" <| (call Count <| opList [get "x"]) 
// let (env3,r3) = evalOp env2 (Repeat(Var(Global,"x"),Var(Global,"a")))
// r3
x |%> passes |> normalizeOp |> evalOp standardCall Map.empty<_,_>


// let threepasses = [passes;passes;passes]
// let threepassestwo = unfoldOp passes (get "a")
// let total = call Total <| opList threepasses
// let total2 = call Total <|  threepassestwo

// evalOp env passes 
// evalOp env total 
// evalOp env total2 
// let three = Value(Int(3))
// evalOp env2 (call Product <| opList [passes;passes]) |> snd


// let op2 = get "A"

// let d6 =  DPlus (D6, 3)

// evalOp env2 (call Product <| opList [Value(d6);three])


// let attacker = 
//     Let("M",vInt 6, Let("MeleeRange", Let("Total", App(Call Total, Value(ParamArray([get "M"; vInt 12]))), Var "Total"), get "Total"))
// attacker |> normalizeOp |> evalOp Map.empty<_,_> 
