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
      Size = 28<mm>
      Attributes = []
      Scale = "scale(1,1)"
      Rules = noValue
      EvaluatedRules = noValue}

let initMeq name coreRules =
    let attributes = [vInt 6; dPlus D6 3 ;dPlus D6 3; vInt 4; vInt 4; vInt 1; vInt 2; vInt 8; dPlus D6 3; Value(NoValue)] 
    let rules = applyArgs coreRules attributes
    { (init name) with Rules = rules; Attributes = attributes }, Cmd.none
let initGeq name coreRules =
    let attributes = [vInt 30; vInt 5; dPlus D6 4 ;dPlus D6 4; vInt 3; vInt 3; vInt 1; vInt 1; vInt 6; dPlus D6 5; Value(NoValue)] 
    let rules = applyArgs (coreRules |> lam "WeaponRange") attributes
    { (init name) with Rules = rules; Attributes = attributes }, Cmd.none
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
      | Value (DPlus _ ) as op -> op
      | Value _ as op -> op
      | Let (n, v, op) -> Let(n, subst arg s v, subst arg s op)
      | Call _ as op -> op

let rec allIds = function
    | Var (v) -> Set.singleton v
    | Lam (p, x) -> Set.add p (allIds x)
    | App (f, a) -> Set.union (allIds f) (allIds a)
    | Value( ManyOp( OpList ops))  -> List.fold(fun s op -> op |> allIds |> Set.union s) Set.empty<_> ops 
    | Call _ | Value _ -> Set.empty<_>
    | Let (n, v, op) -> allIds op |> Set.add n

let freeIds x =
    let rec halp bound = function
        | Var (v) -> if Set.contains v bound then Set.empty else Set.singleton v
        | Lam (p, x) -> halp (Set.add p bound) x
        | App (f, a) -> Set.union (halp bound f) (halp bound a)
        | Value( ManyOp( OpList ops)) -> List.fold(halp) bound ops 
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
    let rec halpGP = function 
        | DPlus(_) 
        | Dice(_) 
        | Int(_) 
        | Dist(_) 
        | NoValue 
        | Str(_) ->  Fine
        | Result(_) -> failwith "Not Implemented"
        | ManyOp(OpList ops) -> 
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
    and halp = function
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
        | Call _ -> Fine
        | Value v -> halpGP v

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
                              | Result(Fail (Int(n))) -> []
                              | Int (n) 
                              | Result(Tuple(Int(n),_))
                              | Result(Pass (Int(n))) -> List.init n (fun _ -> op) 
                              | Result(List xs) -> 
                                    let n = 
                                        List.fold (fun c elem -> 
                                                    match elem with 
                                                    | Pass _ -> c + 1 
                                                    | Fail _ -> c 
                                                    | Tuple(Int(x),_) -> c + x 
                                                    | _ -> failwith <| sprintf "Cannot unfold by these types of values %A" elem) 0 xs
                                    List.init n  (fun _ -> op) 
                              | elem -> failwith <| sprintf "Cannot unfold by these types of values %A" elem
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
      | Int _ as i -> always i  |> Dist |> Value
      | Dice d -> evalDie d  |> Distribution.map (Int)  |> Dist |> Value
      | NoValue -> always NoValue  |> Dist |> Value
      | DPlus(d, moreThan) -> evalDie d |> dPlusTest moreThan |> Distribution.map(Result.map(Int) >> Result) |> Dist |> Value
      | Dist d -> Dist d |> Value
      
      | ManyOp(OpList ops) -> 
            ops
            |> List.fold(fun acc op -> let newOp = evalOp env op
                                       (newOp::acc)) [] 
            |> List.rev 
            |> OpList |> ManyOp |> Value
      | Str _ as s -> always s  |> Dist |> Value
      | Result _ as r -> always r |> Dist |> Value
and evalCall func v env  =
    match func,v with 
    | Total, Value(ManyOp(OpList(ops))) -> 
        always GamePrimitive.Zero
        |> Dist
        |> Value 
        |> fold (+) env ops
    | Product, Value(ManyOp(OpList(ops))) -> 
        Int 1
        |> Pass
        |> Result
        |> always 
        |> Dist
        |> Value 
        |> fold (*) env ops
    | Count, Value(ManyOp(OpList(ops))) ->
        let toCount result = 
            match result with | Result(Pass _) -> Result(Tuple (Int(1),Int(0))) | Result(Fail _) ->  Result(Tuple(Int(0),(Int(1)))) | Result(Tuple _) as x -> x | x -> failwith <| sprintf "Cannot count these %A" x
        let zero = GamePrimitive.Zero 
        Tuple(zero,zero)
        |> Result 
        |> always 
        |> Dist 
        |> Value
        |> fold (fun r1 r2 -> toCount r1 + toCount r2)  env ops
    | Unfold, Value(ManyOp(OpList [op;op2])) -> unfold op op2 env
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
            let evaluated = model.Rules |> normalizeOp |> evalOp initial

            //let cmds = newEnv |> Map.toList |> List.map (fun (name,result) -> Cmd.ofMsg (Msg.Let(name,result)))
            let cmds = []
            { model with EvaluatedRules = evaluated }, Cmd.batch cmds
