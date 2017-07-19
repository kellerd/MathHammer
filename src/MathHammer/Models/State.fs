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
                      "Test", Let(env,"Test", Var(Attacker, "WS"))
                      "MeleeRange", Let(env, "MeleeRange", Total <| OpList [Var(env,"M");Value(Dice(D6));Value(Dice(D6));Value(Dice(D6))])
                      "Psychic", Let(env, "Psychic", Let(env, "PsychicResult", Total <| OpList [Value(Dice(D6));Value(Dice(D6))]))
                      "Melee",  hitMelee
                      "Shots", shotsMelee ] 
                      |> List.mapi(fun i (k,op) -> k,(i,op)) |> Map.ofList }, Cmd.none
let initGeq name env =
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
                      "Test", Let(env,"Test", Var(Attacker, "WS"))
                      "MeleeRange", Let(env, "MeleeRange", Total <| OpList [Var(env,"M");Value(Dice(D6));Value(Dice(D6));Value(Dice(D6))])
                      "Psychic", Let(env, "Psychic", Let(env, "PsychicResult", Total <| OpList [Value(Dice(D6));Value(Dice(D6))])) 
                      "Melee", hitMelee
                      "Shots", shotsMelee
                      "ShootingRange", Let(env, "ShootingRange", Total <| OpList [Value(Int(6))])]
                      |> List.mapi(fun i (k,op) -> k,(i,op)) |> Map.ofList }, Cmd.none
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
      | Int i -> always i |> Distribution.map Pass
      | Dice d -> reduceDie d  |> Distribution.map Pass
      | NoValue -> always 0  |> Distribution.map Fail
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
      | NoResult -> reduce env (Value(NoValue))           
      | Deterministic d -> d
      | NonDeterministic _ -> reduce env (Value(NoValue))
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
and reduce (env:Environment) (operation:Operation) = 
      match operation with
      | Value v -> (env,reduceGamePrimitive v) 
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
      | Var (scope,var)  -> env,(Map.tryFind (scope,var) env |> function Some v -> v | None -> reduce env (Value(NoValue)) |> snd )
      | Let(scope, var, op) -> 
            let (newEnv,result) = reduce env op
            Map.add (scope,var) result newEnv, result

let rec subst = function
| arg, s, Var (scope,v) -> if (scope,v) = s then arg else Var (scope,v)
| arg, s, App (f, a) -> App(subst (arg,s,f), subst (arg,s,a))
| arg, s, Lam (sc, p, x) -> if (sc,p) = s then Lam (sc,p,x) else Lam(sc,p, subst(arg,s,x))

type ExprResult = Normal | Next of Operation


let rec allIds = function
    | Var (sc, v) -> Set.singleton (sc,v)
    | Lam (sc, p, x) -> Set.add (sc,p) (allIds x)
    | App (f, a) -> Set.union (allIds f) (allIds a)

let freeIds x =
    let rec halp bound = function
        | Var (sc, v) -> if Set.contains (sc,v) bound then Set.empty else Set.singleton (sc,v)
        | Lam (sc,p, x) -> halp (Set.add (sc,p) bound) x
        | App (f, a) -> Set.union (halp bound f) (halp bound a)
    halp Set.empty x
type ConflictResult =
    | Fine
    | Renamed of Operation

let isDigit c = match c with | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true | _ -> false 
let idNum (s:string) =
    let rec halp i =
        if i = -1 || not (isDigit s.[i])
        then i
        else halp (i - 1)
    let stop = s.Length - 1
    let res = halp stop
    if res = stop
    then s, 1
    else s.[0 .. res], int(s.[res + 1 .. stop])

let uniqueId taken (sc,s) =
    let prefix, start = idNum s
    let rec halp i =
        let newId = sc,(prefix + string(i))
        if Set.contains newId taken
        then halp (i + 1)
        else newId
    halp (start + 1)
let rename all (t, s, x) =
    let free = freeIds t
    let rec halp = function
        | Var (sc, v) -> Fine
        | App (f, a) ->
            match halp f with
                | Renamed rf -> Renamed (App (rf, a))
                | _ ->
                    match halp a with
                        | Renamed ra -> Renamed (App (f, ra))
                        | _ -> Fine
        | Lam (sc, p, b) ->
            if ((sc, p) = s) || (not (Set.contains s (freeIds b))) || (not (Set.contains (sc, p) free))
            then Fine
            else
                let (sc,newP) = uniqueId all (sc,p)
                Renamed (Lam (sc, newP, subst (Var (sc,newP), (sc,p), b)))

    match halp x,s with
        | Renamed x,(sc,s) -> Renamed (App (Lam (sc, s, x), t))
        | _ -> Fine

let reduce'' x =
      let all = allIds x
      let rec halp = function
            | Var _ -> Normal
            | App (Lam (sc, p, b), a) ->
                  let redex = a, (sc,p), b
                  match rename all redex with
                      | Renamed x -> Next x
                      | Fine -> Next (subst redex)
            | App (f, a) ->
                  match halp f with
                      | Next rf -> Next (App (rf, a))
                      | _ ->
                          match halp a with
                              | Next ra -> Next (App (f, ra))
                              | _ -> Normal
            | Lam (sc, p, b) ->
                  match halp b with
                      | Next b -> Next (Lam (sc, p, b))
                      | _ -> Normal
      halp x





let update msg model =
      match msg with
      | ChangePosition (x,y,scale) -> {model with PosX = x; PosY = y; Scale=scale}, Cmd.none
      | Select _ -> model, Cmd.none
      | Msg.Let _ ->  model, Cmd.none
      | Rebind (scope,initial) -> 
            let newEnv = 
                  model.Attributes 
                  |> Map.toList
                  |> List.sortBy (fun (_,(ord,_)) -> ord)
                  |> List.fold(fun env -> snd >> snd >> reduce env >> fst) initial
            let cmds = newEnv |> Map.filter (fun (scope,name) _ -> scope = Global) |> Map.toList |> List.map (fun ((scope,name),result) -> Cmd.ofMsg (Msg.Let(scope,name,result)))
            { model with Environment = newEnv }, Cmd.batch cmds
