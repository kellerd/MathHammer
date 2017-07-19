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
let WS = Let(Attacker, "WS", DPlus(D6,3))
let Psychic = Let(Attacker, "Psychic", Total(OpList[Value(Dice(D6));Value(Dice(D6))]))
let result = reduce Map.empty<_,_> Psychic |> snd
open Distribution
open Determinism
open MathHammer.Types

let x = Let(Global, "x", DPlus(D6,3))
let a = Let(Global, "a", Value(Int(3)))
let (env,r) = reduce Map.empty<_,_> x
let (env2,r2) = reduce env a

let passes = Count(OpList[Var(Global,"x")])
// let (env3,r3) = reduce env2 (Repeat(Var(Global,"x"),Var(Global,"a")))
// r3
let threepasses = [passes;passes;passes]
let threepassestwo = Unfold(passes,Var(Global, "a"))
let total = Total <| OpList threepasses
let total2 = Total threepassestwo

reduce env2 passes |> snd
reduce env2 total |> snd
reduce env2 total2 |> snd
let three = Value(Int(3))
reduce env2 (Product <| OpList [passes;passes]) |> snd


open Distribution.Example
open Distribution


let op2 = Var(Global, "a")

let d6 =  DPlus (D6, 3)

reduce env2 (Product <| OpList [d6;three])