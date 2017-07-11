#r "../packages/Fable.Core/lib/netstandard1.6/Fable.Core.dll"
#r "../packages/Fable.Elmish/lib/netstandard1.6/Fable.Elmish.dll"
#r "../packages/Fable.Elmish.React/lib/netstandard1.6/Fable.Elmish.React.dll"
#r "../packages/Fable.Elmish.Browser/lib/netstandard1.6/Fable.Elmish.Browser.dll"
#r "../packages/Fable.React/lib/netstandard1.6/Fable.React.dll"
#load "../src/Result/Result.fs"
#load "../src/Probability/Distribution.fs"
#load "../src/Probability/View.fs"
#load "../src/GameActions/Primitives/Types.fs"
#load "../src/GameActions/Primitives/View.fs"
#load "../src/GameActions/GameActionsList/Types.fs"
#load "../src/GameActions/GameActionsList/State.fs"
#load "../src/GameActions/GameActionsList/View.fs"
#load "../src/GameActions/Types.fs"
#load "../src/GameActions/State.fs"
#load "../src/GameActions/View.fs"
#load "../src/MathHammer/Models/Types.fs"
#load "../src/MathHammer/Models/State.fs"
#load "../src/MathHammer/Models/View.fs"
#load "../src/MathHammer/UnitList/Types.fs"
#load "../src/MathHammer/UnitList/State.fs"
#load "../src/MathHammer/UnitList/View.fs"
#load "../src/MathHammer/Types.fs"
#load "../src/MathHammer/State.fs"
#load "../src/MathHammer/View.fs"
#load "../src/Global.fs"
#load "../src/Types.fs"
#load "../src/State.fs"


open App.State
open Probability
open Result
open GameActions.Primitives.Types
open MathHammer.State
open Probability.View
let hitMelee = 
    Let(Attacker, "Melee", Let(Attacker, "MeleeHits", Count[Product[Var(Attacker, "WS"); Var(Attacker, "A")]]))
let A = Let(Attacker, "A", Value(Int(6)))
let WS = Let(Attacker, "WS", DPlus(D6,3))
let Psychic = Let(Attacker, "Psychic", Total[Value(Dice(D6));Value(Dice(D6))])
let Psychic = Let(Attacker, "Product", Product[Value(Dice(D6));Value(Dice(D6))])
let result = reduce Map.empty<_,_> Psychic |> snd

open Distribution
let rec reduce env operation = 
      match operation with
      | Value v -> env,reduceGamePrimitive v |> Distribution.map pass
      | NoValue -> env,always (fail 0)
      | Total ([]) -> env,always (pass 0)
      | Total (op::rest) -> 
            let state = reduce env op
            rest 
            |> List.fold (fun (env1,reduced1) op -> 
                  let (env2,reduced2) = reduce env op
                  env2, dist {
                        let! a' = reduced1
                        let! b' = reduced2
                        return a' + b'                              
                  }) state
      | Product ([]) -> env,always (pass 0)
      | Product (op::rest) -> 
            let state = reduce env op
            rest 
            |> List.fold (fun (env1,reduced1) op -> 
                  let (env2,reduced2) = reduce env op
                  env2, dist {
                        let! a' = reduced1
                        let! b' = reduced2
                        return a' * b'                              
                  }) state
      | DPlus(d, moreThan) -> env,reduceDie d |> dPlus moreThan
      | Unfold (op,op2) ->
            let (env,reduced) = reduce env op
            let (env2,reduced2) = reduce env op2  
            env,dist {
                  let! result = reduced
                  let! result2 = reduced2               
                  return! 
                        match result with
                        | Tuple(n,_) -> List.init n (fun _ -> result * result2) |> uniformDistribution
                        | Pass x -> List.init (int x) (fun _ -> result * result2) |> uniformDistribution
                        | Fail x -> List.init (int x) (fun _ -> result * result2) |> uniformDistribution
                        | List xs -> 
                              let n = List.fold (fun c elem -> match elem with Pass _ -> c + 1 | Fail _ -> c | Tuple(x,_) -> c + x | List _ -> failwith "Cannot count these") 0 xs
                              List.init n (fun _ -> result * result2) |> uniformDistribution
            }            
      | Count ([]) -> env,always (pass 0)
      | Count (op::rest) -> 
            let toCount (env1,result) =
                  env1, dist {
                        let! result = result 
                        return match result with | Pass _ -> Tuple (1,0) | Fail _ ->  Tuple(0,1) | _ -> failwith "Cannot count these" 
                  }
            let state = reduce env op |> toCount
            rest 
            |> List.fold (fun (env1,reduced1) op -> 
                  let (env2,reduced2) = reduce env1 op |> toCount
                  env2,dist {
                        let! count1 = reduced1
                        let! count2 = reduced2 
                        return count1 + count2
                  }) state    
      | Var (scope,var)  -> env,(Map.tryFind (scope,var) env |> function Some v -> v | None -> reduce env NoValue |> snd )
      | Let(scope, var, op) -> 
            let (newEnv,result) = reduce env op
            Map.add (scope,var) result newEnv, result

let x = Let(Global, "x", DPlus(D6,3))
let a = Let(Global, "a", Value(Int(3)))
let (env,r) = reduce Map.empty<_,_> x
let (env2,r2) = reduce env a

let passes = Count[Var(Global,"x")]
// let (env3,r3) = reduce env2 (Repeat(Var(Global,"x"),Var(Global,"a")))
// r3
let total = Total[passes;passes;passes]

reduce env2 passes |> snd
reduce env2 total |> snd
reduce env2 (Product[passes;passes;passes])

List.init (int 3.) (fun _ -> Pass 1.) |> uniformDistribution