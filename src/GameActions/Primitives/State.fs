module GameActions.Primitives.State

open Types
let get v = Var (v)
let noValue = NoValue |> Value
let single op = ParamArray(List.singleton op)
let opList ops = ParamArray(ops)
let pair x y = opList [x;y]
let call f op = App(Call f, op)
let count v = v |> call Count
let unfoldOp op op2 = opList [op; op2] |> call Unfold
let total v = v |> call Total
let product v = v |> call Product
let bindVal text op = Let(text,op,Var text)

let bindOp v op inBody = Operation.Let(v,op, inBody)
let lam s op = Lam (s,op)
let ``D#`` d = App(Call(Dice(d)), noValue)
let d6 = App(Call(Dice(D6)), noValue)
let d3 = App(Call(Dice(D3)), noValue)
let vStr s = Str s |> Value

let dPlus d v = [``D#`` d; Value(Int(v - 1))] |> opList |> call GreaterThan
let vInt i = Value(Int(i))
let emptyOp = noValue
let label v o = pair (vStr v) o
let labelVar v = pair (vStr v) (get v)
let getp s op = PropertyGet(s, op)
let apply f x = App(f,x)
let (|%>) x f = apply f x
let (>>=) o s = bindOp s o
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
          labelVar "ShootingRange"
          labelVar "PsychicTest"
          labelVar "HitResults"
          labelVar "ChargeRange"
          labelVar "MeleeRange"
          labelVar "WoundResults"
          labelVar "D6Test"
          labelVar "D3Test" ]

let hitResults = get "WS" |> single |> total >>= "HitResults"
let svtOps = [get "S";getp "T" (get "Defender")] |> opList >>= "SvsT"

let  table ifThen = 
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
let woundResults = unfoldOp (svtOps sVsT) (get "HitResults") >>= "WoundResults"
let chargeRange = [d6;d6] |> opList |> total >>= "ChargeRange"
let meleeRange = opList [ get "M"; get "ChargeRange" ] |> total >>= "MeleeRange"
let shootingRange = get "WeaponRange" >>= "ShootingRange"
let psychicTest = [d6;d6] |> opList |> total >>= "PsychicTest"
let d6Test = [d6] |> opList |> total >>= "D6Test"
let d3Test = [d3] |> opList |> total >>= "D3Test"



let rec subst arg s = function
      | Var (v) -> if (v) = s then arg else Var (v)
      | App (f, a) -> App(subst arg s f, subst arg s a)
      | Lam (p, x) -> if p = s then Lam (p,x) else Lam(p, subst arg s x)
      | ParamArray( ops) -> ParamArray(List.map (subst arg s) ops)
      | Value v -> Value v
      | Let (n, v, op) -> Let(n, subst arg s v, subst arg s op)
      | Call x -> Call x
      | PropertyGet(a, op) -> PropertyGet(a, subst arg s op)
      | IfThenElse(ifExpr, thenExpr, elseExpr) -> IfThenElse(subst arg s ifExpr, subst arg s thenExpr, elseExpr |> Option.map (fun op -> subst arg s op) )
let rec allIds = function
    | Var (v) -> Set.singleton v
    | Lam (p, x) -> Set.add p (allIds x)
    | App (f, a) -> Set.union (allIds f) (allIds a)
    | ParamArray( ops)  -> List.fold(fun s op -> op |> allIds |> Set.union s) Set.empty<_> ops 
    | Call _ | Value _ -> Set.empty<_>
    | Let (n, v, op) -> allIds op |> Set.add n
    | PropertyGet(_,op) -> allIds op
    | IfThenElse(ifExpr, thenExpr, elseExpr) -> 
        [ allIds ifExpr |> Some
          allIds thenExpr |> Some
          elseExpr |> Option.map allIds ]
        |> List.choose id
        |> List.reduce Set.union

let freeIds x =
    let rec halp bound = function
        | Var (v) -> if Set.contains v bound then Set.empty else Set.singleton v
        | Lam (p, x) -> halp (Set.add p bound) x
        | App (f, a) -> Set.union (halp bound f) (halp bound a)
        | ParamArray( ops) -> List.fold(halp) bound ops 
        | Call _ | Value _ -> bound
        | Let (sc, _ , op) -> halp bound op
        | PropertyGet(_, op) -> halp bound op
        | IfThenElse(ifExpr, thenExpr, elseExpr) -> 
            [ halp bound  ifExpr |> Some
              halp bound  thenExpr |> Some
              elseExpr |> Option.map (halp bound)  ]
            |> List.choose id
            |> List.reduce Set.union
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
        | PropertyGet(p,op) ->
                          match halp op with 
                          | Renamed ro -> Renamed(PropertyGet(p,ro))
                          | _ -> Fine
        | Call _ -> Fine
        | Value v -> Fine
        | ParamArray(ops) -> 
            ops 
            |> List.map (fun op -> op, halp op)
            |> List.fold (fun state (op,rop) -> 
                          match state,rop with
                          | Renamed(ParamArray(ops)),_ -> Renamed(ParamArray((op::ops)))
                          | Renamed (_) as r,_ -> r
                          | Fine,Renamed rop -> Renamed(ParamArray([rop]))
                          | Fine,Fine -> Fine) Fine 
            |> function 
            | Renamed(ParamArray(ops)) -> Renamed(ParamArray(List.rev ops))
            | Renamed (_) as r -> r   
            | _ -> Fine
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
    let (|FirstIsName|_|) = function | ParamArray([Value(Str name');v]) when name' = name -> Some v | _ -> None
    match operation with 
    | FirstIsName v -> Some v
    | ParamArray(ops) -> List.tryPick (function FirstIsName v -> Some v | _ -> None) ops
    | _ -> None        
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
        | PropertyGet (p, b) ->
              match reduce b with
                  | Next b ->  Next (PropertyGet (p, b))
                  | _ -> 
                        match tryFindLabel p b with 
                        | Some op -> Next(op) 
                        | None -> Normal
        | ParamArray(ops) -> 
            let (rops,expr) = 
                ops 
                |> List.map (fun op -> match reduce op with 
                                                  | Normal -> op,Normal
                                                  | Next rop -> rop,Next rop)
                |> List.unzip                            
            if List.exists(function Next _ -> true | Normal -> false) expr then
                Next (ParamArray(rops))
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

let rec evalOp evalCall env (operation:Operation) = 
      //let all = allIds operation
      match operation with
      
      | ParamArray(ops) -> 
            ops
            |> List.fold(fun acc op -> let newOp = evalOp evalCall env op
                                       (newOp::acc)) [] 
            |> List.rev 
            |> ParamArray 
      | Value _ as v -> v
      | Call _ as c -> c
      | Var (var)  -> Map.tryFind (var) env |> function Some v -> v | None -> printfn "Couldn't find value %s" var; noValue
      | Let(str, var, op) -> 
            let result = evalOp evalCall env var
            evalOp evalCall (Map.add str result env) op
      | PropertyGet(str, op) -> 
            let result = evalOp evalCall env op
            match tryFindLabel str result with 
            | Some result -> result
            | None -> noValue
      | Lam _ as l -> l
      | IfThenElse(gp, thenPart, elsePart) -> 
        match evalOp evalCall env gp, elsePart with 
        | (Value(Check(Check.Pass(_)))),_ -> evalOp evalCall env thenPart
        | (Value(Check(Check.Fail(_))),Some elsePart) -> evalOp evalCall env elsePart
        | (Value(Check(Check.Fail(_))),None) -> noValue
        | gp -> failwith <| sprintf "Invalid type, cannot check if/else with %A" gp    
      | App(f, value) -> 
           match evalOp evalCall env f, evalOp evalCall env value with 
           | (Call f),v -> evalCall f v env 
           | _ -> failwith "Cannot apply to something not a function"
// let attackerLam = <@ 
//     let m = 6
//     let a = 5
//     ["M",m; "A",a] @>
// let meq = 
//     emptyOp
//     |> chargeRange
//     |> (vInt 5 |~> "M") 

// let test char = get char |> single |> count
// let hitMelee = 
//     get Attacker "A"
//     |> unfoldOp (test "WS")
//     |> total 
//     |> bindOp Attacker "Melee"

// let shotsMelee = 
//     [Var(Attacker, "A"); Var(Attacker, "A")]
//     |> opList 
//     |> product
//     |> single
//     |> count
//     |> bindOp Attacker "Shots"

// let dPlus d v = Value(DPlus(d,v))
// let vInt i = Value(Int(i))
// let d6 = Value(Dice(D6))

// let chargeRange = [d6;d6] |> opList |> total |> bindOp Attacker "ChargeRange"
// let meleeRange = [get Attacker "M"; chargeRange] |> opList |> total |> bindOp Attacker "MeleeRange"
// let psychicTest = [d6;d6] |> opList |> total |> bindOp Attacker "PsychicTest"

// let sUser = get Attacker "S"
// let r = 
//  <@
//     let claws (a:int) (b:int) (c:int) = 
//         let x = 5
//         let y = 6
//         x + 7 + y
//     claws 5 3 1 @>
//   Let (claws,
//      Lambda (a,
//              Lambda (b,
//                      Lambda (c,
//                              Let (x, Value (5),
//                                   Let (y, Value (6),
//                                        Call (None, op_Addition,
//                                              [Call (None, op_Addition,
//                                                     [x, Value (7)]), y])))))),
//      Application (Application (Application (claws, Value (5)), Value (3))