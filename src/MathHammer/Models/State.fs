module MathHammer.Models.State

open Elmish
open Types
open GameActions.Primitives.Types
open Distribution
open Result

let wsTest = Call <| Count(OpList[Var(Attacker, "WS")])
let hitMelee = 
    Let(Attacker, "Melee", Let(Attacker, "MeleeHits", Call <| Total(Unfold(wsTest, Var(Attacker, "A")))))
let shotsMelee = 
    Let(Attacker, "Shots", Let(Attacker, "Shots", Call(Count <| OpList [Call (Product <| OpList [Var(Attacker, "A"); Var(Attacker, "A")])])))

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
                      "WS", Let(env, "WS", Call(DPlus (D6, 3)))
                      "BS", Let(env, "BS", Call(DPlus (D6, 3)))
                      "S" , Let(env, "S" , Value (Int(4)))
                      "T" , Let(env, "T" , Value (Int(4)))
                      "W" , Let(env, "W" , Value (Int(1)))
                      "A" , Let(env, "A" , Value (Int(2)))
                      "LD", Let(env, "LD", Value (Int(8)))
                      "SV", Let(env, "SV", Call(DPlus (D6, 3)))
                      "Test", Let(env,"Test", Var(Attacker, "WS"))
                      "MeleeRange", Let(env, "MeleeRange", Call (Total <| OpList [Var(env,"M");Value(Dice(D6));Value(Dice(D6));Value(Dice(D6))]))
                      "Psychic", Let(env, "Psychic", Let(env, "PsychicResult", Call(Total <| OpList [Value(Dice(D6));Value(Dice(D6))])))
                      "Melee",  hitMelee
                      "Shots", shotsMelee ] 
                      |> List.mapi(fun i (k,op) -> k,(i,op)) |> Map.ofList }, Cmd.none
let initGeq name env =
    { (init name) with
        Attributes = ["M",  Let(env, "M",  Value(Int(6)))
                      "WS", Let(env, "WS", Call(DPlus (D6, 3)))
                      "BS", Let(env, "BS", Call(DPlus (D6, 3)))
                      "S" , Let(env, "S" , Value (Int(4)))
                      "T" , Let(env, "T" , Value (Int(4)))
                      "W" , Let(env, "W" , Value (Int(1)))
                      "A" , Let(env, "A" , Value (Int(2)))
                      "LD", Let(env, "LD", Value (Int(8)))
                      "SV", Let(env, "SV", Call(DPlus (D6, 3)))
                      "Test", Let(env,"Test", Var(Attacker, "WS"))
                      "MeleeRange", Let(env, "MeleeRange", Call(Total <| OpList [Var(env,"M");Value(Dice(D6));Value(Dice(D6));Value(Dice(D6))]))
                      "Psychic", Let(env, "Psychic", Let(env, "PsychicResult", Call(Total <| OpList [Value(Dice(D6));Value(Dice(D6))])) )
                      "Melee", hitMelee
                      "Shots", shotsMelee
                      "ShootingRange", Let(env, "ShootingRange", Call(Total <| OpList [Value(Int(6))]))]
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
let rec subst arg s = function
      | Var (scope,v) -> if (scope,v) = s then arg else Var (scope,v)
      | App (f, a) -> App(subst arg s f, subst arg s a)
      | Lam (sc, p, x) -> if (sc,p) = s then Lam (sc,p,x) else Lam(sc,p, subst arg s x)
      | Call(Total(OpList ops)) -> List.map (subst arg s) ops |> OpList |> Total |> Call
      | Call(Count(OpList ops)) -> List.map (subst arg s) ops |> OpList |> Count |> Call
      | Call(Product(OpList ops)) -> List.map (subst arg s) ops |> OpList |> Product |> Call
      | Call(Total(Unfold(op,op2))) -> Call(Total(Unfold(subst arg s op, subst arg s op2)))
      | Call(Count(Unfold(op,op2)))  -> Call(Count(Unfold(subst arg s op, subst arg s op2))) 
      | Call(Product(Unfold(op,op2)))  -> Call(Product(Unfold(subst arg s op, subst arg s op2)))
      | Call(DPlus _ ) as op -> op
      | Value _ as op -> op
      | Let(sc, v, op) -> Let(sc,v,subst arg s op)

let rec allIds = function
    | Var (sc, v) -> Set.singleton (sc,v)
    | Lam (sc, p, x) -> Set.add (sc,p) (allIds x)
    | App (f, a) -> Set.union (allIds f) (allIds a)
    | Call(Total(OpList ops)) 
    | Call(Count(OpList ops))  
    | Call(Product(OpList ops)) -> List.fold(fun s op -> op |> allIds |> Set.union s) Set.empty<_> ops 
    | Call(DPlus _ ) -> Set.empty<_>
    | Call(Total(Unfold(op,op2)))
    | Call(Count(Unfold(op,op2)))  
    | Call(Product(Unfold(op,op2)))  -> Set.union (allIds op) (allIds op2)
    | Value(_) -> Set.empty<_>
    | Let(sc, n, op) -> allIds op |> Set.add (sc,n)

let freeIds x =
    let rec halp bound = function
        | Var (sc, v) -> if Set.contains (sc,v) bound then Set.empty else Set.singleton (sc,v)
        | Lam (sc,p, x) -> halp (Set.add (sc,p) bound) x
        | App (f, a) -> Set.union (halp bound f) (halp bound a)
        | Call(Total(OpList ops)) 
        | Call(Count(OpList ops))  
        | Call(Product(OpList ops)) -> List.fold(halp) bound ops 
        | Call(DPlus _ ) -> bound
        | Call(Total(Unfold(op,op2)))
        | Call(Count(Unfold(op,op2)))  
        | Call(Product(Unfold(op,op2)))  -> Set.union (halp bound op) (halp bound op2)
        | Value(_) -> bound
        | Let(sc, _, op) -> halp bound op
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
                Renamed (Lam (sc, newP, subst (Var (sc,newP)) (sc,p) b))
        | Let(sc,v,op) -> match halp op with 
                          | Renamed ro -> Renamed(Let(sc,v,ro))
                          | _ -> Fine

    match halp x,s with
        | Renamed x,(sc,s) -> Renamed (App (Lam (sc, s, x), t))
        | _ -> Fine

type ExprResult<'a> = Normal of 'a | Next of Operation

let rec unfold callback op op2 env = 
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
                        reduce env (Call(callback(OpList newOps)))
            ) 
      match newTimes |> fromDistribution with 
      | NoResult -> reduce env (Value(NoValue))           
      | Deterministic d -> d
      | NonDeterministic _ -> reduce env (Value(NoValue))
and fold  (folder: Result<int>->Result<int>->Result<int>)  state ops env =
      let state = (env,always state)
      ops 
      |> List.fold (fun (env1,reduced1) op -> 
            let (env2,reduced2) = reduce env1 op
            env2, dist {
                  let! a' = reduced1
                  let! b' = reduced2
                  return folder a' b'                              
            }) state
and doManyOp  ops (unfoldf) (foldf) state = 
      match ops with 
      | Unfold (op,op2) -> unfold unfoldf op op2
      | OpList (ops) -> fold foldf state ops
and call func =
      match func with 
      | Total (manyOp) -> doManyOp manyOp Total (Result.add) (Pass 0)
      | Product (manyOp) -> doManyOp manyOp Product (Result.mult) (Pass 1)
      | Count (manyOp) -> doManyOp manyOp Count (Result.count) (Tuple(0,0))
      | DPlus(d, moreThan) -> fun env -> env,reduceDie d |> dPlus moreThan
and reduce (env:Environment) (operation:Operation) = 
      //let all = allIds operation
      match operation with
      | Value v -> (env,reduceGamePrimitive v) 
      | Call(func) -> call func env
      | Var (scope,var)  -> env,(Map.tryFind (scope,var) env |> function Some v -> v | None -> reduce env (Value(NoValue)) |> snd )
      | Let(scope, var, op) -> 
            let (newEnv,result) = reduce env op
            Map.add (scope,var) result newEnv, result


// let reduce'' x =
//       let rec halp = function
//             | Var _ -> Normal
//             | App (Lam (sc, p, b), a) ->
//                   let redex = a, (sc,p), b
//                   match rename all redex with
//                       | Renamed x -> Next x
//                       | Fine -> Next (subst redex)
//             | App (f, a) ->
//                   match halp f with
//                       | Next rf -> Next (App (rf, a))
//                       | _ ->
//                           match halp a with
//                               | Next ra -> Next (App (f, ra))
//                               | _ -> Normal
//             | Lam (sc, p, b) ->
//                   match halp b with
//                       | Next b -> Next (Lam (sc, p, b))
//                       | _ -> Normal
//       halp x





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
