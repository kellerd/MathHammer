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
let largest n ops = call Largest (Value(ParamArray(Value(Int n)::ops)))
let least n ops = call Least (Value(ParamArray(Value(Int n)::ops)))
let lam s op = Lam (s,op)
let ``D#`` d = App(Call Dice, Value(Int d))
let vStr s = Str s |> Value
let vInt i = Value(Int(i))
let emptyOp = noValue
let label v o = pair (vStr v) o
let getp s op = PropertyGet(s, op)
let labelVar v = pair (vStr v) (get v)
let labelProp c v = pair (vStr v) (getp v (get c))
let apply f x = App(f,x)
let (<*>) = apply  
let (>>=) o s = bindOp s o
let dPlus sides plusValue =
    let gt = [Var "roll"; Value(Int(plusValue))] |> opList |> call GreaterThan
    let eq = [Var "roll"; Value(Int(plusValue))] |> opList |> call Equals
    let gte = [Var "eq"; Var "gt" ]  |> opList |> call Or

    let dp = Let ("roll", ``D#`` sides, (Let("gt", gt, Let ("eq", eq,  gte))))
    dp |> count
let (|IsDPlus|_|) = function
    | App
        (Call Count, Let (roll,App (Call Dice,Value (Int die)),
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
let applyMany argList operationBody argValues = 
    applyArgs (createArgs argList operationBody) argValues
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
    [ get "SvsT" |> call GreaterThan, dPlus 6 3 
      get "SvsT" |> call LessThan, dPlus 6 5
      get "SvsT" |> call Equals, dPlus 6 4 ]
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
type ChoiceSet = Map<string,Set<string>>
type ConflictResult =
    | Fine of ChoiceSet
    | Renamed of ChoiceSet * Operation
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
let getChoices (key,choices) = 
    let keys = List.map (fst) choices |> Set.ofList
    Map.add key keys Map.empty<_,_>
let rename all (t, s, x) =
    let choicesOf = function Fine c -> c | Renamed (c,_) -> c
    let free = freeIds t
    let rec halp = function
        | Var (_) -> Fine Map.empty<_,_>
        | App (f, a) ->
            match halp f with
            | Renamed (c, rf) -> Renamed (c, App (rf, a))
            | Fine c ->
                match halp a with
                | Renamed (c2,ra) -> Renamed (Map.mergeSets c c2, App (f, ra))
                | Fine c2 -> Fine (Map.mergeSets c c2) 
        | Lam (p, b) ->
            let c  = halp b |> choicesOf 
            if ((p) = s) || (not (Set.contains s (freeIds b))) || (not (Set.contains (p) free))
            then Fine c
            else
                let newP = uniqueId all p
                Renamed (c, Lam (newP, subst (Var newP) p b))
        | Let(sc,v,op) -> 
            let c = halp v |> choicesOf
            match halp op with 
                          | Renamed (c2,ro) -> Renamed(Map.mergeSets c c2,Let(sc,v,ro))
                          | Fine c2 -> Fine (Map.mergeSets c c2)
        | PropertyGet(p,op) ->
                          match halp op with 
                          | Renamed (c,ro) -> Renamed(c,PropertyGet(p,ro))
                          | Fine c -> Fine c
        | Choice(name,choices) -> 
            choices 
            |> List.map (fun (key,op) -> op,key,halp op)
            |> List.fold (fun state (op,key,rop) -> 
                          match state,rop with
                          | Renamed(c, Choice(name,choices)),(Fine c2 | Renamed(c2,_)) -> Renamed(Map.mergeSets c c2, Choice(name,(key,op)::choices))
                          | Renamed (c,r),(Fine c2 | Renamed(c2,_)) -> Renamed (Map.mergeSets c c2,r)
                          | Fine c,Renamed (c2, rop) -> Renamed(Map.mergeSets c c2, Choice(name,[key,rop]))
                          | Fine c,Fine c2 -> Fine (Map.mergeSets c c2) ) (Fine Map.empty<_,_>)
            |> function 
            | Renamed(c, Choice(name,choices)) -> Renamed(Map.mergeSets (getChoices (name,choices)) c, Choice(name,List.rev choices))
            | Renamed (c, op) -> Renamed(Map.mergeSets (getChoices (name,choices)) c, op)
            | Fine c -> Fine (Map.mergeSets (getChoices (name,choices)) c)
        | Call _ -> Fine Map.empty<_,_>
        | Value v -> 
            let rec halpGp v = 
                match v with 
                | Int(_)               -> Fine Map.empty<_,_>
                | Str(_)               -> Fine Map.empty<_,_>
                | Float(_)             -> Fine Map.empty<_,_>
                | NoValue              -> Fine Map.empty<_,_>
                | Check(Check.Pass gp) -> 
                    match halpGp gp with 
                    | Fine c -> Fine c
                    | Renamed (c, Value(rgp)) -> Renamed(c, Value(Check(Check.Pass rgp)))
                    | gp -> failwith <| sprintf "Could not rename %A" gp
                | Check(Check.Fail gp) -> 
                    match halpGp gp with 
                    | Fine c -> Fine c
                    | Renamed (c, Value(rgp)) -> Renamed(c, Value(Check(Check.Fail rgp)))
                    | gp -> failwith <| sprintf "Could not rename %A" gp
                | Tuple(gp, gp2) -> 
                    match halpGp gp with 
                    | Renamed (c, Value(rgp)) -> Renamed(c, Value(Tuple(rgp, gp2)))
                    | Fine c -> 
                        match halpGp gp2 with 
                        | Renamed (c2, Value(rgp2)) -> Renamed(Map.mergeSets c c2, Value(Tuple(gp, rgp2)))
                        | Fine c2 -> Fine (Map.mergeSets c c2)
                        | gp2 -> failwith <| sprintf "Could not rename %A" gp2
                    | gp -> failwith <| sprintf "Could not rename %A" gp
                | Dist({Probabilities = gps}) -> 
                    gps
                    |> List.map (fun (gp,p) -> (gp,p), halpGp gp)
                    |> List.fold (fun state ((gp,p),rop) -> 
                                  match state,rop with
                                  | Renamed(c, Value(Dist({Probabilities = rgps}))),(Fine c2 | Renamed(c2,_)) -> Renamed(Map.mergeSets c c2, Value(Dist({Probabilities = (gp,p)::rgps})))
                                  | Renamed (c,r),(Fine c2 | Renamed(c2,_)) -> Renamed (Map.mergeSets c c2,r)
                                  | Fine c,Renamed (c2, rop) -> Renamed(Map.mergeSets c c2, Value(Dist({Probabilities = [ParamArray([rop]),p]})))
                                  | Fine c,Fine c2 -> Fine (Map.mergeSets c c2)) (Fine Map.empty<_,_>)
                    |> function 
                    | Renamed(c, Value(Dist({Probabilities = gps}))) -> Renamed(c, Value(Dist({Probabilities = List.rev gps})))
                    | Renamed (_) as r -> r   
                    | Fine c -> Fine c                
                | ParamArray ops -> 
                    ops 
                    |> List.map (fun op -> op, halp op)
                    |> List.fold (fun state (op,rop) -> 
                                  match state,rop with
                                  | Renamed(c, Value(ParamArray(rops))),(Fine c2 | Renamed(c2,_)) -> Renamed(Map.mergeSets c c2, Value(ParamArray((op::rops))))
                                  | Renamed (c,r),(Fine c2 | Renamed(c2,_)) -> Renamed (Map.mergeSets c c2,r)
                                  | Fine c,Renamed (c2, rop) -> Renamed(Map.mergeSets c c2, Value(ParamArray([rop])))
                                  | Fine c,Fine c2 -> Fine (Map.mergeSets c c2)) (Fine Map.empty<_,_>)
                    |> function 
                    | Renamed(c, Value(ParamArray(ops))) -> Renamed(c, Value(ParamArray(List.rev ops)))
                    | Renamed (_) as r -> r   
                    | Fine c -> Fine c
            halpGp v
            
        | IfThenElse(ifExpr, thenExpr, elseExpr) -> 
            match halp ifExpr with
            | Renamed (c,ri) -> Renamed(c,IfThenElse(ri, thenExpr, elseExpr))
            | Fine c ->
                match halp thenExpr, elseExpr |> Option.map(halp) with
                | Renamed (c2,rt), (Some e) -> 
                    let choices =  List.reduceSafe Map.empty<_,_> Map.mergeSets [c;c2;choicesOf e]
                    Renamed(choices, IfThenElse(ifExpr, rt, elseExpr))
                | Renamed (c2,rt), None ->                     
                    Renamed(Map.mergeSets c c2, IfThenElse(ifExpr, rt, elseExpr))
                | Fine c2, Some(Renamed (c3,re))  -> Renamed(List.reduceSafe Map.empty<_,_> Map.mergeSets [c;c2;c3], IfThenElse(ifExpr, thenExpr, Some re))
                | Fine c2, Some(Fine c3)  -> Fine (List.reduceSafe Map.empty<_,_> Map.mergeSets [c;c2;c3])
                | Fine c2, None -> Fine (Map.mergeSets c c2)
    match halp x,s with
        | Renamed (c,x),s -> Renamed (c, App (Lam (s, x), t))
        | Fine c,_ -> Fine c
let tryFindLabel name operation = 
    let (|FirstIsName|_|) = function | Value(ParamArray([Value(Str name');v])) when name' = name -> Some v | _ -> None
    match operation with 
    | FirstIsName v -> Some v
    | Value(ParamArray(ops)) -> List.tryPick (function FirstIsName v -> Some v | _ -> None) ops
    | _ -> None   

type NormalizedResult =  ChoiceSet * NormalizedOperation    


let normalize op : (ChoiceSet * Operation) = 
    let all = freeIds op
    let rec reduce : Operation -> NormalizedResult= function
        | Var _  -> Map.empty<_,_>, Normal
        | Call _ -> Map.empty<_,_>, Normal
        | App (Lam (p, b), a) ->
              let redex = a, p, b
              match rename all redex with
              | Renamed (c,x) -> c, Next x
              | Fine c        -> c, Next (subst a p b)
        | App (f, a) ->
              match reduce f with
              | c, Next rf -> c, Next (App (rf, a))
              | c, Normal  ->
                  match reduce a with
                  | c2, Next ra -> Map.mergeSets c c2, Next (App (f, ra))
                  | c2, Normal  -> Map.mergeSets c c2, Normal
        | Lam (p, b) ->
              match reduce b with
              | c, Next b -> 
                    if not (isInUse p b) then c, Next b
                    else c, Next (Lam (p, b))
              | c, Normal -> 
                    if not (isInUse p b) then c, Next b
                    else c, Normal
        | PropertyGet (p, b) ->
              match reduce b with
              | c, Next b -> c, Next (PropertyGet (p, b))
              | c, Normal -> 
                    match tryFindLabel p b with 
                    | Some op -> c, Next(op) 
                    | None    -> c, Normal
        | Choice(name,choices) -> 
            let newChoices = Map.add name (Set.ofList (choices |> List.map fst)) Map.empty<_,_>
            let (cs,rops,expr) = 
                choices 
                |> List.map (fun (key,op) -> match reduce op with 
                                             | c, Normal   ->  c, (key,op), Normal
                                             | c, Next rop ->  c, (key,rop),Next rop)
                |> List.unzip3                            
            if List.exists(function Next _ -> true | Normal -> false) expr then
                 List.reduceSafe Map.empty<_,_> Map.mergeSets (newChoices::cs), Next (Choice(name,rops))
            else List.reduceSafe Map.empty<_,_> Map.mergeSets (newChoices::cs), Normal                    
        | Value v -> 
            let rec halpGp = function
                | NoValue 
                | Str _ 
                | Float _ 
                | Int _    -> Map.empty<_,_>, Normal
                | Check(Check.Pass gp) -> 
                    match halpGp gp with 
                    | c, Normal -> c, Normal
                    | c, Next (Value(rgp)) -> c, Next(Value(Check(Check.Pass rgp)))
                    | gp -> failwith <| sprintf "Could not normalize %A" gp
                | Check(Check.Fail gp) -> 
                    match halpGp gp with 
                    | c, Normal -> c, Normal
                    | c, Next (Value(rgp)) -> c, Next(Value(Check(Check.Fail rgp)))
                    | gp -> failwith <| sprintf "Could not normalize %A" gp
                | ParamArray(ops) -> 
                    let (cs, rops,expr) = 
                        ops 
                        |> List.map (fun op -> match reduce op with 
                                               | c, Normal ->   c, op,Normal
                                               | c, Next rop -> c, rop,Next rop)
                        |> List.unzip3                            
                    if List.exists(function Next _ -> true | Normal -> false) expr then
                         List.reduceSafe Map.empty<_,_> Map.mergeSets cs, Next (Value(ParamArray(rops)))
                    else List.reduceSafe Map.empty<_,_> Map.mergeSets cs, Normal
                | Tuple(gp, gp2) -> 
                    match halpGp gp with 
                    | c, Next (Value(rgp)) -> c, Next(Value(Tuple(rgp, gp2)))
                    | c, Normal -> 
                        match halpGp gp2 with 
                        | c2, Next (Value(rgp2)) -> (Map.mergeSets c c2), Next(Value(Tuple(gp, rgp2)))
                        | c2, Normal -> (Map.mergeSets c c2), Normal 
                        | gp2 -> failwith <| sprintf "Could not normalize %A" gp2
                    | gp -> failwith <| sprintf "Could not normalize %A" gp
                | Dist({Probabilities = gps}) -> 
                    let (cs, rgps,expr) = 
                        gps 
                        |> List.map (fun (gp,p) -> match halpGp gp with 
                                                   | c, Normal ->   c, (gp,p), Normal
                                                   | c, Next (Value rgp) -> c, (rgp,p),Next (Value(rgp))
                                                   | _ -> failwith <| sprintf "Could not normalize %A" gp)
                        |> List.unzip3                            
                    if List.exists(function Next _ -> true | Normal -> false) expr then
                         List.reduceSafe Map.empty<_,_> Map.mergeSets cs, Next (Value(Dist({Probabilities = rgps})))
                    else List.reduceSafe Map.empty<_,_> Map.mergeSets cs, Normal           
            halpGp v
        | Let(s, v, op) -> 
            match reduce v with
            | c, Next rv -> c, Next ( Let(s, rv, op))
            | c, Normal -> 
                match reduce op with
                | c2, Next rop -> Map.mergeSets c c2, Next ( Let(s, v, rop))
                | c2, Normal   -> Map.mergeSets c c2, Normal
        | IfThenElse(ifExpr, thenExpr, elseExpr) -> 
            match reduce ifExpr with 
            | c, Next ri -> c, Next(IfThenElse(ri, thenExpr, elseExpr))
            | c, Normal -> 
                match reduce thenExpr, elseExpr |> Option.map reduce with 
                | (c2, Next rt), Some(c3,_)           -> List.reduceSafe Map.empty<_,_> Map.mergeSets [c;c2;c3], Next(IfThenElse(ifExpr, rt, elseExpr))
                | (c2, Next rt), None                 -> List.reduceSafe Map.empty<_,_> Map.mergeSets [c;c2   ], Next(IfThenElse(ifExpr, rt, elseExpr))
                | (c2, Normal),  Some (c3, Next(re))  -> List.reduceSafe Map.empty<_,_> Map.mergeSets [c;c2;c3], Next(IfThenElse(ifExpr, thenExpr, Some re))
                | (c2, Normal),  Some (c3, Normal)    -> List.reduceSafe Map.empty<_,_> Map.mergeSets [c;c2;c3], Normal
                | (c2, Normal),  None                 -> List.reduceSafe Map.empty<_,_> Map.mergeSets [c;c2   ], Normal
    Seq.unfold 
        (function 
            | (Next rop) -> 
                let (c2,nextRop) = reduce rop
                Some ((c2, rop), nextRop) 
            | Normal -> None) 
        (Next op)
    |> Seq.last
let rec evalDie n : Distribution.Distribution<_> = 
      match n with 
      | n when n > 0 -> Distribution.uniformDistribution [n .. -1 .. 1]
      | _ -> Distribution.uniformDistribution []
    //   | Reroll(rerolls, d) -> 
    //         Distribution.dist {
    //               let! roll = evalDie d
    //               if List.contains roll rerolls then
    //                     return! evalDie d
    //               else return roll                        
    //         }
let closure env op = 
    let rec fixGp  = function 
        | NoValue 
        | Str _ 
        | Float _
        | Int _ as gp -> gp
        | Check(Check.Fail gp) -> Check.Fail (fixGp gp) |> Check
        | Check(Check.Pass gp) -> Check.Pass (fixGp gp) |> Check
        | ParamArray ops -> List.map (fixOp env) ops |> ParamArray
        | Tuple(gp, gp2) -> Tuple(fixGp gp, fixGp gp2)
        | Dist(gps) -> Distribution.map fixGp gps |> Dist
    and fixOp env = function 
        | Call _ as c -> c
        | Var s -> Map.tryFind s env 
                   |> function Some v -> v
                               | None -> Var s
        | PropertyGet(v, body) -> PropertyGet(v, fixOp env body)
        | App(f, value) -> App(fixOp env f, fixOp env value)
        | Lam(param, body) -> Lam(param, fixOp (Map.remove param env) body)
        | Let(v, value, body) -> 
            let v' = fixOp env value
            Let(v, v' , fixOp (Map.add v v' env) body)
        | Choice(name,choices) -> Choice(name, List.map (fun (key,op) -> key,fixOp env op) choices)
        | IfThenElse(ifExpr, thenExpr, elseExpr) -> IfThenElse(fixOp env ifExpr, fixOp env thenExpr, Option.map (fixOp env) elseExpr)
        | Value v -> Value(fixGp v)
    fixOp env op   
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
let rec evalCall func v env : Operation =
    let repeat lam op2 = 
        let create n = List.init (max 0 n) (fun n -> match lam with | Lam _ -> App(lam,vInt n) | notLam -> notLam
                                                     |> evalOp env) |> opList
        let rec repeatOps times : Operation =
            let rec repeatOp : GamePrimitive->Operation = function
                | NoValue -> create 0
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

        //let times = evalOp env op2  
        match op2 with 
        | Value(gp) -> repeatOps gp
        | _ -> printfn "Times is not a value %A" op2; noValue
    let fold folder ops state =
      ops 
      |> List.fold (fun acc op -> 
            match acc,op with 
            | Value(Dist reduced1),Value(Dist reduced2) -> 
                Distribution.dist {
                      let! a' = reduced1
                      let! b' = reduced2
                      return folder a' b'                             
                } |> Dist |> Value
            | Value(Dist reduced1),Value(b) ->
                Distribution.dist {
                      let! a' = reduced1
                      return folder a' b                             
                } |> Dist |> Value
            |   Value(a), Value(Dist reduced2) ->
                Distribution.dist {
                      let! b' = reduced2
                      return folder a b'                             
                } |> Dist |> Value
            | Value(a), Value(b) ->
                folder a b |> Value
            | _ -> (NoValue |> Value)
            ) state
    let evalFuncAsGp = (fun v -> evalCall func (Value v) env |> function (Value(gp)) -> gp | op -> ParamArray [op])           
    match func,v with 
    | Dice, Value(Int d)                 -> evalDie d |> Distribution.map (Int) |> Dist |> Value 
    | (Total|Division|Product|Max|Min|Sub|Median|Mode), (Value(Int _ | Float _ | Str _ | NoValue) as v)       -> v
    | (Least _|Largest _), (Value(Int _ | Float _ | Str _ | NoValue) as v) -> ParamArray[v] |> Value
    | (Total|Division|Product|Max|Min|Sub|Median|Mode|Least _|Largest _), Value(ParamArray []) ->  GamePrimitive.Zero |> Value
    | Total, Value(ParamArray (h::t))    -> fold (+) t h
    | Max, Value(ParamArray(h::t))       -> fold maxGp t h
    | Min, Value(ParamArray(h::t))       -> fold minGp t h
    | Sub, Value(ParamArray(h::t))       -> fold (-) t h
    | Division, Value(ParamArray(h::t))  -> fold (/) t h
    | Mean, Value(ParamArray(ops))       -> meanOp ops
    | Median, Value(ParamArray(ops))     -> medianGp ops
    | Mode, Value(ParamArray(ops))       -> modeGp ops
    | Least, Value(ParamArray(Value(Int n)::ops))  -> leastOp n ops
    | Largest, Value(ParamArray(Value(Int n)::ops))  -> largestOp n ops
    | Product, Value(ParamArray(h::t))   -> fold (*) t h
    | Count, Value(Int _)                -> vInt 1
    | Count, Value(Str _)                -> vInt 1
    | Count, Value(NoValue)              -> vInt 0 
    | Count, Value(ParamArray [])        -> (GamePrimitive.Zero, GamePrimitive.Zero) |> tuple |> Value
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
        zero |> Value |> fold (fun r1 r2 -> r1 + toCount r2) ops
    | GreaterThan,  Value(ParamArray([Value(gp);Value(gp2)])) -> greaterThan gp gp2 |> Value
    | Equals,       Value(ParamArray([Value(gp);Value(gp2)])) -> equals gp gp2      |> Value
    | LessThan,     Value(ParamArray([Value(gp);Value(gp2)])) -> notEquals gp gp2   |> Value
    | NotEquals,    Value(ParamArray([Value(gp);Value(gp2)])) -> lessThan gp gp2    |> Value
    | And,          Value(ParamArray([Value(gp);Value(gp2)])) -> andGp gp gp2       |> Value
    | Or,           Value(ParamArray([Value(gp);Value(gp2)])) -> orGp gp gp2        |> Value
    | Repeat,       Value(ParamArray([lam;op2]))              -> repeat lam op2 
    | (Total|Division|Product|Count|Max|Min|Sub|Mean|Median|Mode|Least _|Largest _), Value(Check v) -> 
        let chk = Check.map evalFuncAsGp v 
        chk |> Check |> Value 
    | (Total|Division|Product|Count|Max|Min|Sub|Mean|Median|Mode|Least _|Largest _), Value(Tuple(t1,t2)) -> 
        let t1' = evalFuncAsGp t1
        let t2' = evalFuncAsGp t2
        Tuple(t1', t2') |> Value
    | Mean, Value(Dist d) -> 
        let v = d.Probabilities |> List.sumBy (fun (a,p) -> a * (Float p))
        evalCall func (Value v) env 
    | (Total|Division|Product|Count|Max|Min|Sub|Mean|Median|Mode|Least _|Largest _), Value(Dist d) -> 
        let results = 
            Distribution.bind (fun v -> let value = evalCall func (Value v) env 
                                        value |> function 
                                            | Value(Dist(gp)) -> gp
                                            | IsList(Distr _) & Value(ParamArray ops) -> 
                                                ops 
                                                |> List.map(function 
                                                            | Value(Dist(d)) -> d
                                                            | Value(gp) -> Distribution.always (gp)
                                                            | op -> (ParamArray[op]) |> Distribution.always)
                                                |> Distribution.combine
                                            | Value(gp) -> Distribution.always (gp) 
                                            | op -> Distribution.always (ParamArray [op]))  d 
        results |> Dist |> Value
    | (f,x) -> App(Call f,x) //failwith <| sprintf "Invalid call %A\n%A" x (Map.toList env |> List.map fst)
and evalOp env (operation:Operation) : Operation = 
   // printfn "%A" operation
    let (|MadeChoice|_|) env op : Operation option =
        match op with 
        | Choice(key,choices) ->
            Map.tryFind key env
            |> Option.bind(function (Value(Str name)) -> List.tryFind(fst >> (=) name) choices | _ -> None)
            |> Option.map (fun (_,op) -> op)
        | _ -> None
    let r =         
        match operation with
        | Value(ParamArray(ops)) -> 
            ops
            |> List.fold(fun acc op -> let newOp = evalOp env op
                                       (newOp::acc)) [] 
            |> List.rev 
            |> ParamArray 
            |> Value
        | Value _ as v  -> v
        | Call  _ as c  -> c
        | Var (var)  -> 
            Map.tryFind var env 
            |> function Some v -> evalOp env v | None -> Var (var) 
        | Let(str, v, op) -> 
            let result = evalOp env v
            let inner = evalOp (Map.add str result env) op
            inner
        | PropertyGet(str, op) -> 
            let result = evalOp env op
            match tryFindLabel str result with 
            | Some result -> result
            | None -> 
                //printfn "Couldn't find property %s in %A" str op;
                PropertyGet(str, op)
        | Lam _ as l -> closure env l
        | IfThenElse(gp, thenPart, elsePart) -> 
            let result = evalOp env gp
            match result with 
            | Value(Check(Check.Fail(_)) | NoValue) -> elsePart 
            | Value _ -> Some thenPart
            | _ -> None
            |> Option.map (evalOp env)
            |> function 
            | Some (v) -> v
            | None -> noValue
        | MadeChoice env selectedChoice -> 
            let result = evalOp env selectedChoice
            result
        | Choice(name, choices) -> Choice(name, choices)
        | App(Lam(x, op),value) ->
            evalOp (Map.add x value env) op 
        | App(f, value) -> 
            match evalOp env f, evalOp env value with 
            | Call f, v -> evalCall f v env 
            | Lam(x, op), v -> App(Lam(x, op),v) |> evalOp env
            | Var x, _ -> failwith ("Could not find function " + x)
            | f',x' -> failwith <| sprintf "Cannot apply to something not a function App(%A,%A) = %A,%A" f value f' x'
    //printfn "%A" r
    r