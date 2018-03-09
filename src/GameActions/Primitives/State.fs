module GameActions.Primitives.State

open Types
open TypeChecker
let get v = Var (v)
let noValue = NoValue |> Value
let opList ops = ParamArray(ops) |> Value
let pair x y = opList [x;y]
let call f op = App(Call f, op)
let tuple ab = Tuple(ab)
let count = call Count
let repeat ``(fun op -> 't)`` op2 = opList [``(fun op -> 't)``; op2;] |> call Repeat
let repeatOp op = ("unusedVariable", op) |> Lam |> repeat
let total = call Total
let product = call Product
let bindVal text op = Let(text,op,Var text)
let bindOp v op inBody = Let(v,op, inBody)
let lam s op = Lam (s,op)
let ``D#`` d = App(Call(Dice(d)), noValue)
let vStr s = Str s |> Value
let vInt i = Value(Int(i))
let emptyOp = noValue
let label v o = pair (vStr v) o
let getp s op = PropertyGet(s, op)
let labelVar v = pair (vStr v) (get v)
let labelProp c v = pair (vStr v) (getp v (get c))
let apply f x = App(f,x)
let (|%>) x f = apply f x
let (>>=) o s = bindOp s o
let dPlus d v =
    let gt = [Var "roll"; Value(Int(v))] |> opList |> call GreaterThan
    let eq = [Var "roll"; Value(Int(v))] |> opList |> call Equals
    let gte = [Var "eq"; Var "gt" ]  |> opList |> call Or

    let dp = Let ("roll", ``D#`` d, (Let("gt", gt, Let ("eq", eq,  gte))))
    dp |> count
let (|IsDPlus|_|) = function
    | App
        (Call Count, Let (roll,App (Call (Dice die),Value NoValue),
               Let (gt, App (Call GreaterThan,Value (ParamArray [Var roll'; Value (Int d)])),
                    Let (eq, App (Call Equals,Value (ParamArray [Var roll''; Value (Int d')])),
                         App (Call Or,Value (ParamArray [Var eq'; Var gt']))))))
        when roll = roll' && roll' = roll'' &&
             gt = gt' &&
             eq = eq' &&
             d = d'
             -> Some (die,d)
    | _ -> None
let nestOps ops returning = ops |> List.rev |> List.reduce (>>) <| returning
let createArgs argList operationBody = argList  |> List.rev |> List.fold (fun f arg -> lam arg f) operationBody
let applyArgs body argValues = 
    body::argValues
    |> List.reduce apply
//Equations
let allProps = 
    opList 
        [ labelVar "M"
          labelVar "WS"
          labelVar "BS"
          labelVar "S"
          labelVar "T"
          labelVar "W"
          labelVar "A"
          labelVar "Ld"
          labelVar "Sv"
          labelVar "InvSv"
          labelVar "D6Test"
          labelVar "D3Test"
          labelProp "Actions" "ChargeRange"
          labelProp "Actions" "MeleeRange"
          labelProp "Actions" "HitReuslts"
          labelProp "Actions" "WoundResults"
          labelProp "Actions" "UnsavedWounds"
          labelProp "Actions" "PsychicTest"
          labelProp "Actions" "ShootingRange"
          labelProp "Actions" "DenyTest" ]
let choose name choiceList = Choice(name,choiceList)
let defenderMap = "Defender"
let attackerMap = "Attacker"
let getd p = getp p (get "Defender")
let geta p = getp p (get "Attacker")
let svtOps = [get "S";getd "T"] |> opList >>= "SvsT"
let table ifThen = 
    let rec acc x = 
        match x with 
        | [] -> None
        | (cmp,thenPortion)::xs -> IfThenElse(cmp,thenPortion,acc xs) |> Some
    match acc ifThen with 
    | None -> noValue
    | Some o -> o

let sVsT = 
    [ get "SvsT" |> call GreaterThan, dPlus D6 3 
      get "SvsT" |> call LessThan, dPlus D6 5
      get "SvsT" |> call Equals, dPlus D6 4 ]
    |> table   

let rec isInUse s = function
      | Var (v) -> v = s
      | App (f, a) -> isInUse s f || isInUse s a 
      | Lam (p, x) -> p <> s && isInUse s x
      | Value(ParamArray(ops)) -> List.exists (isInUse s) ops
      | Value _ -> false
      | Let (n, v, op) -> isInUse s v || (n <> s && isInUse s op)
      | Call _ -> false
      | PropertyGet(_, op) -> isInUse s op
      | IfThenElse(ifExpr, thenExpr, None) -> isInUse s ifExpr || isInUse s thenExpr
      | IfThenElse(ifExpr, thenExpr, Some elseExpr) -> isInUse s ifExpr || isInUse s thenExpr || isInUse s elseExpr
      | Choice(_, choices) -> List.exists (snd >> isInUse s) choices

let rec subst arg s = function
      | Var (v) -> if (v) = s then arg else Var (v)
      | App (f, a) -> App(subst arg s f, subst arg s a)
      | Lam (p, x) -> if p = s then Lam (p,x) else Lam(p, subst arg s x)
      | Value(ParamArray( ops)) -> Value(ParamArray(List.map (subst arg s) ops))
      | Value v -> Value v
      | Let (n, v, op) -> Let(n, subst arg s v, subst arg s op)
      | Call x -> Call x
      | PropertyGet(a, op) -> PropertyGet(a, subst arg s op)
      | IfThenElse(ifExpr, thenExpr, elseExpr) -> IfThenElse(subst arg s ifExpr, subst arg s thenExpr, elseExpr |> Option.map (fun op -> subst arg s op) )
      | Choice(name, choices) -> Choice(name, List.map (fun (k,op) -> k, subst arg s op) choices)
let rec allIds = function
    | Var (v) -> Set.singleton v
    | Lam (p, x) -> Set.add p (allIds x)
    | App (f, a) -> Set.union (allIds f) (allIds a)
    | Value(ParamArray( ops))  -> List.fold(fun s op -> op |> allIds |> Set.union s) Set.empty<_> ops 
    | Call _ | Value _ -> Set.empty<_>
    | Let (n, _, op) -> allIds op |> Set.add n
    | PropertyGet(_,op) -> allIds op
    | IfThenElse(ifExpr, thenExpr, elseExpr) -> 
        [ allIds ifExpr |> Some
          allIds thenExpr |> Some
          elseExpr |> Option.map allIds ]
        |> List.choose id
        |> List.reduce Set.union
    | Choice(_, choices) -> List.map(snd >> allIds) choices |> List.reduce Set.union
let freeIds x =
    let rec halp bound = function
        | Var (v) -> if Set.contains v bound then Set.empty else Set.singleton v
        | Lam (p, x) -> halp (Set.add p bound) x
        | App (f, a) -> Set.union (halp bound f) (halp bound a)
        | Value(ParamArray( ops)) -> List.fold(halp) bound ops 
        | Call _ | Value _ -> bound
        | Let (_, _ , op) -> halp bound op
        | PropertyGet(_, op) -> halp bound op
        | IfThenElse(ifExpr, thenExpr, elseExpr) -> 
            [ halp bound  ifExpr |> Some
              halp bound  thenExpr |> Some
              elseExpr |> Option.map (halp bound)  ]
            |> List.choose id
            |> List.reduce Set.union
        | Choice(_, choices) -> List.map(snd >> halp bound) choices |> List.reduce Set.union        
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
        | Var (_) -> Fine
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
        | PropertyGet(p,op) ->
                          match halp op with 
                          | Renamed ro -> Renamed(PropertyGet(p,ro))
                          | _ -> Fine
        | Value(ParamArray(ops)) -> 
            ops 
            |> List.map (fun op -> op, halp op)
            |> List.fold (fun state (op,rop) -> 
                          match state,rop with
                          | Renamed(Value(ParamArray(ops))),_ -> Renamed(Value(ParamArray((op::ops))))
                          | Renamed (_) as r,_ -> r
                          | Fine,Renamed rop -> Renamed(Value(ParamArray([rop])))
                          | Fine,Fine -> Fine) Fine 
            |> function 
            | Renamed(Value(ParamArray(ops))) -> Renamed(Value(ParamArray(List.rev ops)))
            | Renamed (_) as r -> r   
            | _ -> Fine
        | Choice(name,choices) -> 
            choices 
            |> List.map (fun (key,op) -> op,key,halp op)
            |> List.fold (fun state (op,key,rop) -> 
                          match state,rop with
                          | Renamed(Choice(name,choices)),_ -> Renamed(Choice(name,(key,op)::choices))
                          | Renamed (_) as r,_ -> r
                          | Fine,Renamed rop -> Renamed(Choice(name,[key,rop]))
                          | Fine,Fine -> Fine ) Fine 
            |> function 
            | Renamed(Choice(name,choices)) -> Renamed(Choice(name,List.rev choices))
            | Renamed (_) as r -> r   
            | _ -> Fine        
        | Call _ -> Fine
        | Value _ -> Fine
        | IfThenElse(ifExpr, thenExpr, elseExpr) -> 
            match halp ifExpr with
            | Renamed ri -> Renamed(IfThenElse(ri, thenExpr, elseExpr))
            | _ ->
                match halp thenExpr, elseExpr |> Option.map(halp) with
                | Renamed rt,_ -> Renamed(IfThenElse(ifExpr, rt, elseExpr))
                | _,Some(Renamed re)  -> Renamed(IfThenElse(ifExpr, thenExpr, Some re))
                | _ -> Fine
    match halp x,s with
        | Renamed x,s -> Renamed (App (Lam (s, x), t))
        | _ -> Fine
let tryFindLabel name operation = 
    let (|FirstIsName|_|) = function | Value(ParamArray([Value(Str name');v])) when name' = name -> Some v | _ -> None
    match operation with 
    | FirstIsName v -> Some v
    | Value(ParamArray(ops)) -> List.tryPick (function FirstIsName v -> Some v | _ -> None) ops
    | _ -> None   
let normalize op = 
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
              | Next b -> 
                    if not (isInUse p b) then Next b
                    else Next (Lam (p, b))
              | _ -> 
                    if not (isInUse p b) then Next b
                    else Normal
        | PropertyGet (p, b) ->
              match reduce b with
              | Next b ->  Next (PropertyGet (p, b))
              | _ -> 
                    match tryFindLabel p b with 
                    | Some op -> Next(op) 
                    | None -> Normal
        | Choice(name,choices) -> 
            let (rops,expr) = 
                choices 
                |> List.map (fun (key,op) -> match reduce op with 
                                             | Normal -> (key,op),Normal
                                             | Next rop -> (key,rop),Next rop)
                |> List.unzip                            
            if List.exists(function Next _ -> true | Normal -> false) expr then
                Next (Choice(name,rops))
            else Normal                    
        | Value(ParamArray(ops)) -> 
            let (rops,expr) = 
                ops 
                |> List.map (fun op -> match reduce op with 
                                       | Normal -> op,Normal
                                       | Next rop -> rop,Next rop)
                |> List.unzip                            
            if List.exists(function Next _ -> true | Normal -> false) expr then
                Next (Value(ParamArray(rops)))
            else Normal
        | Value _ -> Normal
        | Let(s, v, op) -> 
            match reduce v with
            | Next rv -> Next ( Let(s, rv, op))
            | _ -> 
                match reduce op with
                | Next rop -> Next ( Let(s, v, rop))
                | _ -> Normal
        | IfThenElse(ifExpr, thenExpr, elseExpr) -> 
            match reduce ifExpr with 
            | Next ri -> Next(IfThenElse(ri, thenExpr, elseExpr))
            | _ -> 
                match reduce thenExpr, elseExpr |> Option.map reduce with 
                | Next rt, _  -> Next(IfThenElse(ifExpr, rt, elseExpr))
                | _, Some (Next(re)) -> Next(IfThenElse(ifExpr, thenExpr, Some re))
                | _ -> Normal
    Seq.unfold 
        (function Next rop -> Some (rop, reduce rop) | Normal -> None) 
        (Next op)
    |> Seq.last
let rec evalDie d : Distribution.Distribution<_> = 
      match d with 
      | D3 -> Distribution.uniformDistribution [3..-1..1]
      | D6 -> Distribution.uniformDistribution [6..-1..1]
      | Reroll(rerolls, d) -> 
            Distribution.dist {
                  let! roll = evalDie d
                  if List.contains roll rerolls then
                        return! evalDie d
                  else return roll                        
            }
type EvalResult = (Map<string,Set<string>> * Operation)            
let rec evalCall func v env : EvalResult =
    let repeat lam op2 = 
        let create n = List.init (max 0 n) (fun n -> match lam with | Lam _ -> App(lam,vInt n) | notLam -> notLam
                                                     |> evalOp env |> snd) |> opList
        let nestCheck check op = 
            let rec fixGp check = function 
                | NoValue 
                | Str _ 
                | Float _
                | Int _ as gp -> gp |> check
                | Check(Check.Fail gp) -> fixGp (Check.Fail >> Check) gp
                | Check(Check.Pass gp) -> fixGp check gp
                | ParamArray ops -> List.map (fixOp check) ops |> ParamArray
                | Tuple(gp, gp2) -> Tuple(fixGp check gp, fixGp check gp2)
                | Dist(gps) -> Distribution.map (fixGp check) gps |> Dist
            and fixOp check = function 
                | Call _
                | Var _ as gp -> gp
                | PropertyGet(v, body) -> PropertyGet(v, fixOp check body)
                | App(f, value) -> App(fixOp check f, fixOp check value)
                | Lam(param, body) -> Lam(param, fixOp check body)
                | Let(v, value, body) -> Let(v, fixOp check value, fixOp check body)
                | Choice(name,choices) -> Choice(name, List.map (fun (key,op) -> key,fixOp check op) choices)
                | IfThenElse(ifExpr, thenExpr, elseExpr) -> IfThenElse(fixOp check ifExpr, fixOp check thenExpr, Option.map (fixOp check) elseExpr)
                | Value v -> Value(fixGp check v)
            fixOp check op
        let rec repeatOps times : Operation =
            let rec repeatOp : GamePrimitive->Operation = function
                | NoValue -> noValue
                | Check(Check.Fail (m)) -> 
                    repeatOp m |> nestCheck (Check.Fail >> Check)
                | Check(Check.Pass (n)) -> 
                    repeatOp n |> nestCheck (Check.Pass >> Check)
                | Int (n) -> create n
                | Dist(times) -> 
                    times 
                    |> Distribution.bind(fun gp -> 
                        match repeatOps gp with 
                        | Value(Dist(gp)) -> gp 
                        | Value(gp) -> Distribution.always gp 
                        | op -> Distribution.always (ParamArray[op])) |> Dist |> Value
                | Str(_) -> failwith "Not Implemented"
                | Float(_) -> failwith "Not Implemented"
                | ParamArray(times) -> times |> List.map(function Value(gp) -> repeatOps gp | _ -> noValue ) |> ParamArray |> Value
                | Tuple(n, m) -> 
                    match repeatOp n, repeatOp m with 
                    | Value(ParamArray(n)), Value(ParamArray(m)) -> n @ m |> ParamArray
                    | Value(n), Value(m) -> Tuple(n,m)
                    | op,op2 -> [op;op2] |> ParamArray 
                    |> Value
            repeatOp times

        let times = evalOp env op2  
        match times with 
        | choices,Value(gp) -> choices,repeatOps gp
        | _ -> printfn "Times is not a value %A" times; (Map.empty<_,_>,noValue)
    let fold folder ops state : EvalResult=
      ops 
      |> List.fold (fun (choices,reduced1) op -> 
            let (newChoices,reduced2) = evalOp env op
            match reduced1,reduced2 with 
            | Value(Dist reduced1),Value(Dist reduced2) -> 
                (Map.mergeSets newChoices choices),Distribution.dist {
                      let! a' = reduced1
                      let! b' = reduced2
                      return folder a' b'                             
                } |> Dist |> Value
            | Value(Dist reduced1),Value(b) ->
                (Map.mergeSets newChoices choices),Distribution.dist {
                      let! a' = reduced1
                      return folder a' b                             
                } |> Dist |> Value
            |   Value(a), Value(Dist reduced2) ->
                (Map.mergeSets newChoices choices),Distribution.dist {
                      let! b' = reduced2
                      return folder a b'                             
                } |> Dist |> Value
            | Value(a), Value(b) ->
                (Map.mergeSets newChoices choices),folder a b |> Value
            | _ -> Map.empty<_,_>,(NoValue |> Value)
            ) (Map.empty<_,_>,state)
    let evalFuncAsGp = (fun v -> evalCall func (Value v) env |> function (choices,Value(gp)) -> choices,gp | (choices,op) -> choices,ParamArray [op])           
    match func,v with 
    | Dice d, _                          -> Map.empty<_,_>, evalDie d |> Distribution.map (Int) |> Dist |> Value 
    | (Total|Division|Product|Max|Min|Sub|Median|Mode), (Value(Int _ | Float _ | Str _ | NoValue) as v)       -> Map.empty<_,_>, v
    | (Least _|Largest _), (Value(Int _ | Float _ | Str _ | NoValue) as v) -> Map.empty<_,_>, ParamArray[v] |> Value
    | (Total|Division|Product|Max|Min|Sub|Median|Mode|Least _|Largest _), Value(ParamArray []) -> Map.empty<_,_>,  GamePrimitive.Zero |> Value
    | Total, Value(ParamArray (h::t))    -> fold (+) t h
    | Max, Value(ParamArray(h::t))       -> fold maxGp t h
    | Min, Value(ParamArray(h::t))       -> fold minGp t h
    | Sub, Value(ParamArray(h::t))       -> fold (-) t h
    | Division, Value(ParamArray(h::t))  -> fold (/) t h
    | Mean, Value(ParamArray(ops))       -> Map.empty<_,_>, meanOp ops
    | Median, Value(ParamArray(ops))     -> Map.empty<_,_>, medianGp ops
    | Mode, Value(ParamArray(ops))       -> Map.empty<_,_>, modeGp ops
    | Least n, Value(ParamArray(ops))    -> Map.empty<_,_>, leastOp n ops
    | Largest n, Value(ParamArray(ops))  -> Map.empty<_,_>, largestOp n ops
    | Product, Value(ParamArray(h::t))   -> fold (*) t h
    | Count, Value(Int _)                -> Map.empty<_,_>, vInt 1
    | Count, Value(Str _)                -> Map.empty<_,_>, vInt 1
    | Count, Value(NoValue)              -> Map.empty<_,_>, vInt 0 
    | Count, Value(ParamArray [])        -> Map.empty<_,_>, (GamePrimitive.Zero, GamePrimitive.Zero) |> tuple |> Value
    | Count, Value(ParamArray ops) ->
        let rec toCount result = 
            match result with 
            | Int _               -> Int(1) 
            | Float _             -> Int(1)
            | Str _               -> Int(1) 
            | NoValue             -> Int(0)     
            | Dist d              -> Distribution.map (toCount) d |> Dist                  
            | ParamArray []       -> Int(0)
            | ParamArray ops      -> ops |> List.map (function | Value(gp) -> toCount gp |> Value | op -> op ) |> ParamArray
            | Check(Check.Pass gp)-> Check.Pass (toCount gp) |> Check
            | Tuple (n, m)        -> tuple(toCount n,toCount m)
            | Check(Check.Fail gp) -> Check.Fail (toCount gp) |> Check
        let zero = GamePrimitive.Zero 
        zero
        |> Value
        |> fold (fun r1 r2 -> r1 + toCount r2) ops
    | GreaterThan,  Value(ParamArray([Value(gp);Value(gp2)])) -> Map.empty<_,_>, greaterThan gp gp2 |> Value
    | Equals,       Value(ParamArray([Value(gp);Value(gp2)])) -> Map.empty<_,_>, equals gp gp2      |> Value
    | LessThan,     Value(ParamArray([Value(gp);Value(gp2)])) -> Map.empty<_,_>, notEquals gp gp2   |> Value
    | NotEquals,    Value(ParamArray([Value(gp);Value(gp2)])) -> Map.empty<_,_>, lessThan gp gp2    |> Value
    | And,          Value(ParamArray([Value(gp);Value(gp2)])) -> Map.empty<_,_>, andGp gp gp2       |> Value
    | Or,           Value(ParamArray([Value(gp);Value(gp2)])) -> Map.empty<_,_>, orGp gp gp2        |> Value
    | Repeat,       Value(ParamArray([lam;op2]))              -> repeat lam op2 
    | (Total|Division|Product|Count|Max|Min|Sub|Mean|Median|Mode|Least _|Largest _), Value(Check v) -> 
        let chk = Check.map evalFuncAsGp v 
        let value = Check.map snd chk
        Check.(|CheckValue|) chk |> fst, value |> Check |> Value 
    | (Total|Division|Product|Count|Max|Min|Sub|Mean|Median|Mode|Least _|Largest _), Value(Tuple(t1,t2)) -> 
        let (choices,t1') = evalFuncAsGp t1
        let (choices2,t2') = evalFuncAsGp t2
        Map.mergeSets choices choices2, Tuple(t1', t2') |> Value
    | Mean, Value(Dist d) -> 
        let v = d.Probabilities |> List.sumBy (fun (a,p) -> a * (Float p))
        evalCall func (Value v) env 
    | (Total|Division|Product|Count|Max|Min|Sub|Mean|Median|Mode|Least _|Largest _), Value(Dist d) -> 
        let (choices,results) = 
            Distribution.bind (fun v -> let (choices,value) = evalCall func (Value v) env 
                                        value |> function 
                                            | Value(Dist(gp)) -> gp |> Distribution.map(fun gp -> choices,gp)
                                            | IsList(Distr _) & Value(ParamArray ops) -> 
                                                ops 
                                                |> List.map(function 
                                                            | Value(Dist(d)) -> d |> Distribution.map(fun gp -> choices,gp)
                                                            | Value(gp) -> Distribution.always (choices,gp)
                                                            | op -> (choices,ParamArray[op]) |> Distribution.always)
                                                |> Distribution.combine
                                            | Value(gp) -> Distribution.always (choices,gp) 
                                            | op -> Distribution.always (choices, ParamArray [op]))  d 
            |> Distribution.unzip
        (choices |> Distribution.values |> List.reduceSafe Map.empty<_,_> Map.mergeSets), results |> Dist |> Value
    | _ -> failwith "Invalid call"
and evalOp env (operation:Operation) : EvalResult = 
    let getChoices (key,choices) = 
        let keys = List.map (fst) choices |> Set.ofList
        Map.add key keys Map.empty<_,_>
    let (|MadeChoice|_|) env op =
        match op with 
        | Choice(key,choices) ->
            Map.tryFind key env
            |> Option.bind(function (Value(Str name)) -> List.tryFind(fst >> (=) name) choices | _ -> None)
            |> Option.map (fun (_,op) -> getChoices (key,choices), op)
        | _ -> None
    match operation with
    | Value(ParamArray(ops)) -> 
        let (choices,newOps) = 
            ops
            |> List.fold(fun acc op -> let newOp = evalOp env op
                                       (newOp::acc)) [] 
            |> List.rev 
            |> List.unzip
        List.reduceSafe Map.empty<_,_> Map.mergeSets choices, newOps|> ParamArray |> Value
    | Value _ as v  -> Map.empty<_,_>, v
    | Call  _ as c  -> Map.empty<_,_>, c
    | Var (var)  -> 
        Map.tryFind var env 
        |> function Some v -> v | None -> noValue 
        |> evalOp env
    | Let(str, v, op) -> 
        let (choices,result) = evalOp env v
        let (choices2,inner) = evalOp (Map.add str result env) op
        Map.mergeSets choices2 choices, inner
    | PropertyGet(str, op) -> 
        let (choices, result) = evalOp env op
        match tryFindLabel str result with 
        | Some result -> 
            choices, result
        | None -> //printfn "Couldn't find propert %s in %A" str op;
            choices, noValue
    | Lam _ as l -> Map.empty<_,_>,l
    | IfThenElse(gp, thenPart, elsePart) -> 
        let (choices, result) = evalOp env gp
        match result with 
        | Value(Check(Check.Fail(_)) | NoValue) -> elsePart 
        | Value _ -> Some thenPart
        | _ -> None
        |> Option.map (evalOp env)
        |> function 
        | Some (choices2,v) -> Map.mergeSets choices2 choices, v
        | None -> choices, noValue
    | MadeChoice env (choices, selectedChoice) -> 
        let (choices2,result) = evalOp env selectedChoice
        Map.mergeSets choices2 choices, result
    | Choice(name, choices) ->
        (getChoices (name,choices)), Choice(name, choices)
    | App(Lam(x, op),value) ->
        evalOp (Map.add x value env) op 
    | App(f, value) -> 
        match evalOp env f, evalOp env value with 
        | (choices,Call f), (choices2,v) -> 
            let (choices3,result) = evalCall f v env 
            List.reduceSafe Map.empty<_,_> Map.mergeSets [choices;choices2;choices3],result 
        | (_,Lam(x, op)),(_,v) -> App(Lam(x, op),v) |> evalOp env
        | f',x' -> failwith <| sprintf "Cannot apply to something not a function App(%A,%A) = %A,%A" f value f' x'
