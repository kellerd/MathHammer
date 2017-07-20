module MathHammer.Models.State

open Elmish
open Types
open GameActions.Primitives.Types
open Distribution
open Result

let get sc v = Var (sc, v)
let single op = Value(ManyOp(List.singleton op |> OpList))
let opList ops = Value(ManyOp(OpList ops))
let unfoldOp op op2 =  Value(ManyOp(Unfold(op,op2)))
let call f op = App(Call f, op)
let count v = v |> call Count
let total v = v |> call Total
let product v = v |> call Product

let wsTest = get Attacker "WS" |> single |> count
let bind sc v op = Let(sc,v,op)

let hitMelee = 
    get Attacker "A"
    |> unfoldOp wsTest 
    |> total 
    |> bind Attacker "MeleeHits"
    |> bind Attacker "Melee"

let shotsMelee = 
    [Var(Attacker, "A"); Var(Attacker, "A")]
    |> opList 
    |> product
    |> single
    |> count

let dPlus d v = Value(DPlus(d,v))
let vInt i = Value(Int(i))
let d6 = Value(Dice(D6))

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
        Attributes = ["M",  vInt 6 |> bind env "M" 
                      "WS", dPlus D6 3 |> bind env "WS" 
                      "BS", dPlus D6 3 |> bind env "BS" 
                      "S" , vInt 4 |> bind env "S" 
                      "T" , vInt 4 |> bind env "T" 
                      "W" , vInt 1 |> bind env "W" 
                      "A" , vInt 1 |> bind env "A" 
                      "LD", vInt 8 |> bind env "LD" 
                      "SV", dPlus D6 3 |> bind env "SV" 
                      "Test", get Attacker "WS" |> bind env "Test" 
                      "MeleeRange", [get env "M";d6;d6;d6] |> opList |> total |> bind env "MeleeRange"
                      "Psychic", [d6;d6] |> opList |> total |> bind env "Psychic"
                      "Melee",  hitMelee
                      "Shots", shotsMelee ] 
                      |> List.mapi(fun i (k,op) -> k,(i,op)) |> Map.ofList }, Cmd.none
let initGeq name env =
    { (init name) with
        Attributes = ["M",  vInt 6 |> bind env "M" 
                      "WS", dPlus D6 3 |> bind env "WS" 
                      "BS", dPlus D6 3 |> bind env "BS" 
                      "S" , vInt 4 |> bind env "S" 
                      "T" , vInt 4 |> bind env "T" 
                      "W" , vInt 1 |> bind env "W" 
                      "A" , vInt 1 |> bind env "A" 
                      "LD", vInt 8 |> bind env "LD" 
                      "SV", dPlus D6 3 |> bind env "SV" 
                      "Test", get Attacker "WS" |> bind env "Test" 
                      "MeleeRange", [get env "M";d6;d6;d6] |> opList |> total |> bind env "MeleeRange"
                      "Psychic", [d6;d6] |> opList |> total |> bind env "Psychic"
                      "Melee",  hitMelee
                      "Shots", shotsMelee
                      "ShootingRange", vInt 6 |> single |> total |> bind env "ShootingRange" ] 
                      |> List.mapi(fun i (k,op) -> k,(i,op)) |> Map.ofList }, Cmd.none
let dPlusTest plus die = dist {
      let! roll = die
      let result = 
            if roll >= plus then Pass roll
            else Fail roll
      return result
}
let d6d = uniformDistribution [1..6]
let d3d = uniformDistribution [1..3]
let rec evalDie d : Distribution<_> = 
      match d with 
      | D3 -> d3d
      | D6 -> d6d
      | Reroll(rerolls, d) -> 
            dist {
                  let! roll = evalDie d
                  if List.contains roll rerolls then
                        return! evalDie d
                  else return roll                        
            }
     
let rec subst arg s = function
      | Var (scope,v) -> if (scope,v) = s then arg else Var (scope,v)
      | App (f, a) -> App(subst arg s f, subst arg s a)
      | Lam (sc, p, x) -> if (sc,p) = s then Lam (sc,p,x) else Lam(sc,p, subst arg s x)
      | Value( ManyOp( OpList ops)) -> Value( ManyOp(List.map (subst arg s) ops |> OpList))
      | Value( ManyOp(Unfold(op,op2)))  ->  Value( ManyOp(Unfold(subst arg s op, subst arg s op2)))
      | Value (DPlus _ ) as op -> op
      | Value _ as op -> op
      | Let (sc, v, op) -> Let(sc,v,subst arg s op)
      | Call _ as op -> op

let rec allIds = function
    | Var (sc, v) -> Set.singleton (sc,v)
    | Lam (sc, p, x) -> Set.add (sc,p) (allIds x)
    | App (f, a) -> Set.union (allIds f) (allIds a)
    | Value( ManyOp( OpList ops))  -> List.fold(fun s op -> op |> allIds |> Set.union s) Set.empty<_> ops 
    | Value( ManyOp(Unfold(op,op2)))  -> Set.union (allIds op) (allIds op2)
    | Call _ | Value _ -> Set.empty<_>
    | Let (sc, n, op) -> allIds op |> Set.add (sc,n)

let freeIds x =
    let rec halp bound = function
        | Var (sc, v) -> if Set.contains (sc,v) bound then Set.empty else Set.singleton (sc,v)
        | Lam (sc,p, x) -> halp (Set.add (sc,p) bound) x
        | App (f, a) -> Set.union (halp bound f) (halp bound a)
        | Value( ManyOp( OpList ops)) -> List.fold(halp) bound ops 
        | Value( ManyOp(Unfold(op,op2)))  -> Set.union (halp bound op) (halp bound op2)
        | Call _ | Value _ -> bound
        | Let (sc, _ , op) -> halp bound op
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
        | Value (ManyOp(Unfold(op,op2))) -> 
            match halp op with
            | Renamed rop -> Renamed (Value (ManyOp(Unfold(rop,op2))))
            | _ ->
                match halp op2 with
                | Renamed rop2 -> Renamed (Value(ManyOp(Unfold(op,rop2))))
                | _ -> Fine
        | Value (ManyOp(OpList ops)) -> 
            ops 
            |> List.map (fun op -> op, halp op)
            |> List.fold (fun state (op,rop) -> 
                          match state,rop with
                          | Renamed(Value (ManyOp(OpList ops))),_ -> Renamed(Value (ManyOp(OpList (op::ops))))
                          | Renamed (_) as r,_ -> r
                          | Fine,Renamed rop -> Renamed(Value (ManyOp(OpList [rop])))
                          | Fine,Fine -> Fine) Fine 
            |> function 
            | Renamed(Value (ManyOp(OpList ops))) -> Renamed(Value (ManyOp(OpList <| List.rev ops)))
            | Renamed (_) as r -> r   
            | _ -> Fine

    match halp x,s with
        | Renamed x,(sc,s) -> Renamed (App (Lam (sc, s, x), t))
        | _ -> Fine
let normalizeOp op = 
    let all = freeIds op
    let rec reduce = function
        | Var _ -> Normal
        | App (Lam (sc, p, b), a) ->
              let redex = a, (sc,p), b
              match rename all redex with
                  | Renamed x -> Next x
                  | Fine -> Next (subst a (sc,p) b)
        | App (f, a) ->
              match reduce f with
                  | Next rf -> Next (App (rf, a))
                  | _ ->
                      match reduce a with
                          | Next ra -> Next (App (f, ra))
                          | _ -> Normal
        | Lam (sc, p, b) ->
              match reduce b with
                  | Next b -> Next (Lam (sc, p, b))
                  | _ -> Normal
        | Value(ManyOp(Unfold(op,op2))) -> 
            match reduce op with
                  | Next rop -> Next (Value(ManyOp(Unfold(rop,op2))))
                  | _ ->
                      match reduce op2 with
                          | Next rop2 -> Next (Value(ManyOp(Unfold(op,rop2))))
                          | _ -> Normal
        | Value(ManyOp(OpList ops)) -> 
            let (rops,expr) = 
                ops 
                |> List.map (fun op -> match reduce op with 
                                                  | Normal -> op,Normal
                                                  | Next rop -> rop,Next rop)
                |> List.unzip                            
            if List.exists(function Next _ -> true | Normal -> false) expr then
                Next (Value(ManyOp(OpList rops)))
            else Normal
        | Value _ -> Normal
        | Let(sc, b, a) -> 
            match reduce a with
            | Next a -> Next (Lam (sc, b, a))
            | _ -> Normal
    Seq.unfold 
        (function Next rop -> Some (rop, reduce rop) | Normal -> None) 
        (Next op)
    |> Seq.last
let evalGamePrimitive = function
      | Int i -> always i |> Distribution.map Pass
      | Dice d -> evalDie d  |> Distribution.map Pass
      | NoValue -> always 0  |> Distribution.map Fail
      | DPlus(d, moreThan) -> evalDie d |> dPlusTest moreThan
open Determinism 
let rec unfold f op op2 env = 
      let (env,times) = evalOp env op2  
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
                        evalOp env (App(Call f, Value(ManyOp(OpList newOps))))
            ) 
      match newTimes |> fromDistribution with 
      | NoResult -> evalOp env (Value(NoValue))           
      | Deterministic d -> d
      | NonDeterministic _ -> evalOp env (Value(NoValue))
and fold  (folder: Result<int>->Result<int>->Result<int>)  state ops env =
      let state = (env,always state)
      ops 
      |> List.fold (fun (env1,reduced1) op -> 
            let (env2,reduced2) = evalOp env1 op
            env2, dist {
                  let! a' = reduced1
                  let! b' = reduced2
                  return folder a' b'                              
            }) state
and doManyOp  ops (unfoldf) (foldf) state = 
      match ops with 
      | Unfold (op,op2) -> unfold unfoldf op op2
      | OpList (ops) -> fold foldf state ops
and evalCall func manyOp =
    match func with 
    | Total -> (Result.add),(Pass 0)
    | Product -> (Result.mult),(Pass 1)
    | Count -> (Result.count),(Tuple(0,0))
    ||> doManyOp manyOp func 

and evalOp (env:Environment) (operation:Operation) = 
      //let all = allIds operation
      match operation with
      | Value v -> env,evalGamePrimitive v 
      | Call f -> env, evalGamePrimitive (NoValue)  
      | Var (scope,var)  -> env,(Map.tryFind (scope,var) env |> function Some v -> v | None -> evalOp env (Value(NoValue)) |> snd )
      | Let(scope, var, op) -> 
            let (newEnv,result) = evalOp env op
            Map.add (scope,var) result newEnv, result
      | App (Call f, Value(ManyOp(ops))) -> evalCall f ops env    
      | Lam _ -> env, evalGamePrimitive (NoValue)     


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
                  |> List.fold(fun env -> snd >> snd >> evalOp env >> fst) initial
            let cmds = newEnv |> Map.filter (fun (scope,name) _ -> scope = Global) |> Map.toList |> List.map (fun ((scope,name),result) -> Cmd.ofMsg (Msg.Let(scope,name,result)))
            { model with Environment = newEnv }, Cmd.batch cmds
