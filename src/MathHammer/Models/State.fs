module MathHammer.Models.State

open Elmish
open Types
open GameActions.Primitives.Types
open Distribution
open Result

let wsTest = Count(OpList[Var(Attacker, "WS")])
let hitMelee = 
    Let(Attacker, "Melee", Let(Attacker, "MeleeHits", Total(Unfold(wsTest, Var(Attacker, "A")))))
let shotsMelee = 
    Let(Attacker, "Shots", Let(Attacker, "Shots", Count <| OpList [Product <| OpList [Var(Attacker, "A"); Var(Attacker, "A")]]))

let init name =
    { PosX=0.
      PosY=0.
      Name=name
      Attributes = Map.empty<_,_>
      Size = 28<mm>
      Scale = "scale(1,1)"
      Environment = Map.empty<_,_>}

let initMeq name env =
    { (init name) with 
        Attributes = ["M",  Let(env, "M",  Value(Int(6)))
                      "WS", Let(env, "WS", DPlus (D6, 3))
                      "BS", Let(env, "BS", DPlus (D6, 3))
                      "S" , Let(env, "S" , Value (Int(4)))
                      "T" , Let(env, "T" , Value (Int(4)))
                      "W" , Let(env, "W" , Value (Int(1)))
                      "A" , Let(env, "A" , Value (Int(2)))
                      "LD", Let(env, "LD", Value (Int(8)))
                      "SV", Let(env, "SV", DPlus (D6, 3))
                      "MeleeRange", Let(env, "MeleeRange", Total <| OpList [Var(env,"M");Value(Dice(D6));Value(Dice(D6));Value(Dice(D6))])
                      "Psychic", Let(env, "Psychic", Let(env, "PsychicResult", Total <| OpList [Value(Dice(D6));Value(Dice(D6))])) 
                      "Melee", hitMelee
                      "Shots",shotsMelee ] |> Map.ofList }, Cmd.none
let initGeq name env =
    { (init name) with
        Attributes = ["M",  Let(env, "M",  Value(Int(5)))
                      "WS", Let(env, "WS", DPlus (D6, 4))
                      "BS", Let(env, "BS", DPlus (D6, 4))
                      "S" , Let(env, "S" , Value (Int(3)))
                      "T" , Let(env, "T" , Value (Int(3)))
                      "W" , Let(env, "W" , Value (Int(1)))
                      "A" , Let(env, "A" , Value (Int(1)))
                      "LD", Let(env, "LD", Value (Int(7)))
                      "SV", Let(env, "SV", DPlus (D6, 5))
                      "ShootingRange", Let(env, "ShootingRange", Total <| OpList [Value(Int(6))])]  |> Map.ofList }, Cmd.none
let dPlus plus die = dist {
      let! roll = die
      let result = 
            if roll >= plus then Pass roll
            else Fail roll
      return result
}
let d6 = uniformDistribution [1..6]
let d3 = uniformDistribution [1..3]
let rec reduceDie d : Distribution<_> = 
      match d with 
      | D3 -> d3
      | D6 -> d6
      | Reroll(rerolls, d) -> 
            dist {
                  let! roll = reduceDie d
                  if List.contains roll rerolls then
                        return! reduceDie d
                  else return roll                        
            }
let reduceGamePrimitive = function
      | Int i -> always i
      | Dice d -> reduceDie d 
open Determinism      

let rec unfold callback env op op2  = 
      let (env,times) = reduce env op2  
      let newTimes = 
            times |> Distribution.map(fun times' ->
                        let newOps = 
                              match times' with  
                              | Tuple(n,_) -> List.init n (fun _ -> op) 
                              | Pass x -> List.init (int x)  (fun _ -> op) 
                              | Fail x -> List.init (int x) (fun _ -> op) 
                              | List xs -> 
                                    let n = List.fold (fun c elem -> match elem with Pass _ -> c + 1 | Fail _ -> c | Tuple(x,_) -> c + x | List _ -> failwith "Cannot count these") 0 xs
                                    List.init n  (fun _ -> op) 
                        reduce env (callback(OpList newOps))
            ) 
      match newTimes |> fromDistribution with 
      | NoResult -> reduce env NoValue             
      | Deterministic d -> d
      | NonDeterministic _ -> reduce env NoValue  
and fold (folder: Result<int>->Result<int>->Result<int>) env state ops =
      let state = (env,always state)
      ops 
      |> List.fold (fun (env1,reduced1) op -> 
            let (env2,reduced2) = reduce env1 op
            env2, dist {
                  let! a' = reduced1
                  let! b' = reduced2
                  return folder a' b'                              
            }) state
and reduce (env:Environment) operation = 
      match operation with
      | Value v -> (env,reduceGamePrimitive v |> Distribution.map Pass) 
      | NoValue -> (env,always (Fail 0)) 
      | DPlus(d, moreThan) -> env,reduceDie d |> dPlus moreThan
      | Total (Unfold(op,op2)) -> unfold Total env op op2 
      | Total (OpList ops) -> fold (Result.add) env (Pass 0) ops 
      | Product (Unfold(op,op2)) -> unfold Product env op op2 
      | Product (OpList ops) -> fold (Result.mult) env (Pass 1) ops
      | Count (Unfold(op,op2)) -> unfold Count env op op2 
      | Count (OpList ops) -> 
            let addCounts r1 r2 =
                  let toCount result =  
                        match result with | Pass _ -> Tuple (1,0) | Fail _ ->  Tuple(0,1) | Tuple _ as x -> x | _ -> failwith "Cannot count these" 
                  Result.add r1 (toCount r2)                      
            fold addCounts env (Tuple(0,0)) ops 
      | Var (scope,var)  -> env,(Map.tryFind (scope,var) env |> function Some v -> v | None -> reduce env NoValue |> snd )
      | Let(scope, var, op) -> 
            let (newEnv,result) = reduce env op
            Map.add (scope,var) result newEnv, result

let update msg model =
      match msg with
      | ChangePosition (x,y,scale) -> {model with PosX = x; PosY = y; Scale=scale}, Cmd.none
      | Select _ -> model, Cmd.none
      | Msg.Let _ ->  model, Cmd.none
      | Rebind scope -> 
            printfn "Got msg to rebind"
            let (newEnv,cmdMap) = 
                  model.Attributes 
                  |> Map.fold (fun env _ -> reduce env >> fst) Map.empty<_,_>
                  |> Map.partition (fun (scope',_) _ -> scope = scope')
            let cmds = cmdMap |> Map.toList |> List.map (fun ((scope,name),result) -> Cmd.ofMsg (Msg.Let(scope,name,result)))
            { model with Environment = newEnv }, Cmd.batch cmds
