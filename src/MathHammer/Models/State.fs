module MathHammer.Models.State

open Elmish
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.State
open Distribution
open Result

let init name =
    { PosX=0.
      PosY=0.
      Name=name
      Attributes = Map.empty<_,_>
      Size = 28<mm>
      Scale = "scale(1,1)"
      Environment = Map.empty<_,_>}

let initMeq name =
    { (init name) with 
        Attributes = Map.empty<_,_>
        // Attributes = ["M",  vInt 6 |> bindVar env "M" 
        //               "WS", dPlus D6 3 |> bindVar env "WS" 
        //               "BS", dPlus D6 3 |> bindVar env "BS" 
        //               "S" , vInt 4 |> bindVar env "S" 
        //               "T" , vInt 4 |> bindVar env "T" 
        //               "W" , vInt 1 |> bindVar env "W" 
        //               "A" , vInt 1 |> bindVar env "A" 
        //               "LD", vInt 8 |> bindVar env "LD" 
        //               "SV", dPlus D6 3 |> bindVar env "SV" 
        //               "MeleeRange", meleeRange
        //               "ChargeRange", chargeRange
        //               "PsychicTest", psychicTest ] 
        }, Cmd.none
let initGeq name =
    { (init name) with
        Attributes = Map.empty<_,_>
        //              ["M",  vInt 6 |> bindVar env "M" 
        //               "WS", dPlus D6 4 |> bindVar env "WS" 
        //               "BS", dPlus D6 4 |> bindVar env "BS" 
        //               "S" , vInt 3 |> bindVar env "S" 
        //               "T" , vInt 3 |> bindVar env "T" 
        //               "W" , vInt 1 |> bindVar env "W" 
        //               "A" , vInt 1 |> bindVar env "A" 
        //               "LD", vInt 7 |> bindVar env "LD" 
        //               "SV", dPlus D6 5 |> bindVar env "SV" 
        //               "MeleeRange", meleeRange
        //               "ChargeRange", chargeRange
        //               "PsychicTest", psychicTest
        //               "ShootingRange", vInt 6 |> single |> total |> bindVar env "ShootingRange" ] 
        }, Cmd.none
let dPlusTest plus die = dist {
      let! roll = die
      let result = 
            if roll >= plus then Pass roll
            else Fail roll
      return result
}
let rec evalDie d : Distribution<_> = 
      match d with 
      | D3 -> uniformDistribution [1..3]
      | D6 -> uniformDistribution [1..6]
      | Reroll(rerolls, d) -> 
            dist {
                  let! roll = evalDie d
                  if List.contains roll rerolls then
                        return! evalDie d
                  else return roll                        
            }
     
let rec subst arg s = function
      | Var (v) -> if (v) = s then arg else Var (v)
      | App (f, a) -> App(subst arg s f, subst arg s a)
      | Lam (p, x) -> if p = s then Lam (p,x) else Lam(p, subst arg s x)
      | Value( ManyOp( OpList ops)) -> Value( ManyOp(List.map (subst arg s) ops |> OpList))
      | Value( ManyOp(Unfold(op,op2)))  ->  Value( ManyOp(Unfold(subst arg s op, subst arg s op2)))
      | Value (DPlus _ ) as op -> op
      | Value _ as op -> op
      | Let (n, v, op) -> Let(n, subst arg s v, subst arg s op)
      | Call _ as op -> op

let rec allIds = function
    | Var (v) -> Set.singleton v
    | Lam (p, x) -> Set.add p (allIds x)
    | App (f, a) -> Set.union (allIds f) (allIds a)
    | Value( ManyOp( OpList ops))  -> List.fold(fun s op -> op |> allIds |> Set.union s) Set.empty<_> ops 
    | Value( ManyOp(Unfold(op,op2)))  -> Set.union (allIds op) (allIds op2)
    | Call _ | Value _ -> Set.empty<_>
    | Let (n, v, op) -> allIds op |> Set.add n

let freeIds x =
    let rec halp bound = function
        | Var (v) -> if Set.contains v bound then Set.empty else Set.singleton v
        | Lam (p, x) -> halp (Set.add p bound) x
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

let uniqueId taken s =
    let prefix, start = idNum s
    let rec halp i =
        let newId = (prefix + string(i))
        if Set.contains newId taken
        then halp (i + 1)
        else newId
    halp (start + 1)
let rename all (t, s, x) =
    let free = freeIds t
    let rec halp = function
        | Var (v) -> Fine
        | App (f, a) ->
            match halp f with
            | Renamed rf -> Renamed (App (rf, a))
            | _ ->
                match halp a with
                | Renamed ra -> Renamed (App (f, ra))
                | _ -> Fine
        | Lam (p, b) ->
            if ((p) = s) || (not (Set.contains s (freeIds b))) || (not (Set.contains (p) free))
            then Fine
            else
                let newP = uniqueId all p
                Renamed (Lam (newP, subst (Var newP) p b))
        | Let(sc,v,op) -> match halp op with 
                          | Renamed ro -> Renamed(Let(sc,v,ro))
                          | _ -> Fine
        | Value (DPlus(_)) 
        | Value (Dice(_)) 
        | Value (Int(_)) 
        | Value (Dist(_)) 
        | Value (NoValue) -> Fine 
        | Call _ -> Fine
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
        | Renamed x,s -> Renamed (App (Lam (s, x), t))
        | _ -> Fine
let normalizeOp op = 
    let all = freeIds op
    let rec reduce = function
        | Var _ -> Normal
        | Call _ -> Normal
        | App (Lam (p, b), a) ->
              let redex = a, p, b
              match rename all redex with
                  | Renamed x -> Next x
                  | Fine -> Next (subst a p b)
        | App (f, a) ->
              match reduce f with
                  | Next rf -> Next (App (rf, a))
                  | _ ->
                      match reduce a with
                          | Next ra -> Next (App (f, ra))
                          | _ -> Normal
        | Lam (p, b) ->
              match reduce b with
                  | Next b -> Next (Lam (p, b))
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
        | Let(s, v, op) -> 
            match reduce v with
            | Next rv -> Next ( Let(s, rv, op))
            | _ -> 
                match reduce op with
                | Next rop -> Next ( Let(s, v, rop))
                | _ -> Normal
    Seq.unfold 
        (function Next rop -> Some (rop, reduce rop) | Normal -> None) 
        (Next op)
    |> Seq.last

open Determinism 
let rec unfold op op2 env = 
    let times = evalOp env op2  
    let newTimes = 
        match times with 
        | Value(Dist(times)) ->
            times |> Distribution.map(fun times' ->
                        let newOps = 
                              match times' with  
                              | Tuple(Int(n),_) 
                              | Pass (Int(n)) 
                              | Fail (Int(n)) -> List.init n (fun _ -> op) 
                              | List xs -> 
                                    let n = 
                                        List.fold (fun c elem -> 
                                                    match elem with 
                                                    | Pass _ -> c + 1 
                                                    | Fail _ -> c 
                                                    | Tuple(Int(x),_) -> c + x 
                                                    | _ -> failwith "Cannot unfold by these types of values") 0 xs
                                    List.init n  (fun _ -> op) 
                              | _ -> failwith "Cannot unfold by these types of values"
                        evalOp env (Value(ManyOp(OpList newOps)))
            ) 
        | _ -> always <| Value(NoValue)
    match newTimes |> fromDistribution with 
    | NoResult -> evalOp env (Value(NoValue))           
    | Deterministic d -> d
    | NonDeterministic _ -> evalOp env (Value(NoValue))
and fold folder env ops state =
      ops 
      |> List.fold (fun reduced1 op -> 
            let reduced2 = evalOp env op
            match reduced1,reduced2 with 
            | Value(Dist reduced1),Value(Dist reduced2) -> 
                dist {
                      let! a' = reduced1
                      let! b' = reduced2
                      return folder a' b'                             
                } |> Dist |> Value
            | _ -> Value(NoValue)
            ) state
and evalGamePrimitive env = function
      | Int i -> always i  |> Distribution.map (Int >> Pass)  |> Dist |> Value
      | Dice d -> evalDie d  |> Distribution.map (Int >> Pass)  |> Dist |> Value
      | NoValue -> always NoValue  |> Distribution.map (Fail)  |> Dist |> Value
      | DPlus(d, moreThan) -> evalDie d |> dPlusTest moreThan |> Distribution.map(Result.map(Int)) |> Dist |> Value
      | Dist d -> Dist d |> Value
      | ManyOp(Unfold (op,op2)) -> unfold op op2 env
      | ManyOp(OpList ops) -> 
            ops
            |> List.fold(fun acc op -> let newOp = evalOp env op
                                       (newOp::acc)) [] 
            |> List.rev 
            |> OpList |> ManyOp |> Value
and evalCall func v env  =
    match func,v with 
    | Total, Value(ManyOp(OpList(ops))) -> 
        Int 0
        |> Pass
        |> always 
        |> Dist
        |> Value 
        |> fold Result.add env ops
    | Product, Value(ManyOp(OpList(ops))) -> 
        Int 1
        |> Pass
        |> always 
        |> Dist
        |> Value 
        |> fold Result.mult  env ops
    | Count, Value(ManyOp(OpList(ops))) -> 
        Value(Dist (always (Tuple(0,0))))
        |> fold Result.count  env ops
    | Total, Value _
    | Product,  Value _ 
    | Count,  Value _ -> evalCall func (Value(ManyOp(OpList [v]))) env
    | _ -> failwith "Cannot call this function with these parameters"
and evalOp (env:Environment) (operation:Operation) = 
      //let all = allIds operation
      match operation with
      | Value v -> evalGamePrimitive env v 
      | Call f -> Call f
      | Var (var)  -> Map.tryFind (var) env |> function Some v -> v | None -> Value(NoValue)
      | Let(str, var, op) -> 
            let result = evalOp env var
            evalOp (Map.add (str) result env) op
      | Lam _ -> Value(NoValue)
      | App(f, value) -> 
           match evalOp env f, evalOp env value with 
           | (Call f),v -> evalCall f v env 
           | _ -> failwith "Cannot apply to something not a function"


let update msg model =
      match msg with
      | ChangePosition (x,y,scale) -> {model with PosX = x; PosY = y; Scale=scale}, Cmd.none
      | Select _ -> model, Cmd.none
      | Msg.Let _ ->  model, Cmd.none
      | Rebind (initial) -> 
            let newEnv = 
                  model.Attributes
                  |> Map.map (fun _ -> normalizeOp >> evalOp initial)

            //let cmds = newEnv |> Map.toList |> List.map (fun (name,result) -> Cmd.ofMsg (Msg.Let(name,result)))
            let cmds = []
            { model with Environment = newEnv }, Cmd.batch cmds
