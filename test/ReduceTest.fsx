#load "LoadModules.fsx"



open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.Models.State
let ee = evalOp Map.empty<_,_>
let A = Value(Int(6)) |~> "A" 
A |> ee
let WS = Value(DPlus(D6,3)) |~> "WS"
WS |> ee
let psychicDice = [d6;d6] |> opList |~> "PsychicDice"
psychicDice |> ee
let psychicTest = psychicDice |> total |~> "PsychicTest"
psychicTest |> ee

open Distribution
open Determinism
open MathHammer.Types

<@ let x = 5
   x @>

let x = dPlus D6 3 |~> "x"
let a = vInt 3 |~> "a"
let (env,r) = evalOp Map.empty<_,_> x
let (env2,r2) = evalOp env a

let passes = call Count <| opList [get "x"] 
// let (env3,r3) = evalOp env2 (Repeat(Var(Global,"x"),Var(Global,"a")))
// r3
let threepasses = [passes;passes;passes]
let threepassestwo = unfoldOp passes (get "a")
let total = call Total <| opList threepasses
let total2 = call Total <|  threepassestwo

evalOp env2 passes |> snd
evalOp env2 total |> snd
evalOp env2 total2 |> snd
let three = Value(Int(3))
evalOp env2 (call Product <| opList [passes;passes]) |> snd


open Distribution.Example
open Distribution

let op2 = get "A"

let d6 =  DPlus (D6, 3)

evalOp env2 (call Product <| opList [Value(d6);three])