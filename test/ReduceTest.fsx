#load "LoadModules.fsx"



open App.State
open Probability
open Result
open GameActions.Primitives.Types
open MathHammer.State
open Probability.View
open MathHammer.Models.State

// let hitMelee = 
//     Let(Attacker, "Melee", Let(Attacker, "MeleeHits", Count[Product[Var(Attacker, "WS"); Var(Attacker, "A")]]))
let A = Let(Attacker, "A", Value(Int(6)))
let WS = Let(Attacker, "WS", Value(DPlus(D6,3)))
let Psychic = Let(Attacker, "Psychic", Call(Total,OpList[Value(Dice(D6));Value(Dice(D6))]))
let result = evalOp Map.empty<_,_> Psychic |> snd
open Distribution
open Determinism
open MathHammer.Types

let x = Let(Global, "x", Value(DPlus(D6,3)))
let a = Let(Global, "a", Value(Int(3)))
let (env,r) = evalOp Map.empty<_,_> x
let (env2,r2) = evalOp env a

let passes = (Call(Count,OpList[Var(Global,"x")]))
// let (env3,r3) = evalOp env2 (Repeat(Var(Global,"x"),Var(Global,"a")))
// r3
let threepasses = [passes;passes;passes]
let threepassestwo = Unfold(passes,Var(Global, "a"))
let total = Call(Total,OpList threepasses)
let total2 = Call(Total,threepassestwo)

evalOp env2 passes |> snd
evalOp env2 total |> snd
evalOp env2 total2 |> snd
let three = Value(Int(3))
evalOp env2 (Call(Product,OpList [passes;passes])) |> snd


open Distribution.Example
open Distribution


let op2 = Var(Global, "a")

let d6 =  DPlus (D6, 3)

evalOp env2 (Call(Product,OpList [Value(d6);three]))