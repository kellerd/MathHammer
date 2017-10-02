module GameActions.Primitives.State

open Types
let get v = Var (v)
let noValue = NoValue |> Value
let single op = ParamArray(List.singleton op |> OpList)
let opList ops = ParamArray(OpList ops)
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
let str s = Str s |> Value

let dPlus d v = [``D#`` d; Value(Int(v - 1))] |> opList |> call GreaterThan
let vInt i = Value(Int(i))
let emptyOp = noValue
let label v o = pair (str v) o
let labelVar v = pair (str v) (get v)
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
          labelVar "D6Test"
          labelVar "D3Test" ]

let hitResults = get "WS" |> single |> total >>= "HitResults"
let chargeRange = [d6;d6] |> opList |> total >>= "ChargeRange"
let meleeRange = opList [ get "M"; get "ChargeRange" ] |> total >>= "MeleeRange"
let shootingRange = opList [get "WeaponRange"] >>= "ShootingRange"
let psychicTest = [d6;d6] |> opList |> total >>= "PsychicTest"
let d6Test = [d6] |> opList |> total >>= "D6Test"
let d3Test = [d3] |> opList |> total >>= "D3Test"
let rec subst arg s = function
      | Var (v) -> if (v) = s then arg else Var (v)
      | App (f, a) -> App(subst arg s f, subst arg s a)
      | Lam (p, x) -> if p = s then Lam (p,x) else Lam(p, subst arg s x)
      | ParamArray( OpList ops) -> ParamArray(List.map (subst arg s) ops |> OpList)
      | Value _ as op -> op
      | Let (n, v, op) -> Let(n, subst arg s v, subst arg s op)
      | Call _ as op -> op
let rec allIds = function
    | Var (v) -> Set.singleton v
    | Lam (p, x) -> Set.add p (allIds x)
    | App (f, a) -> Set.union (allIds f) (allIds a)
    | ParamArray( OpList ops)  -> List.fold(fun s op -> op |> allIds |> Set.union s) Set.empty<_> ops 
    | Call _ | Value _ -> Set.empty<_>
    | Let (n, v, op) -> allIds op |> Set.add n

let freeIds x =
    let rec halp bound = function
        | Var (v) -> if Set.contains v bound then Set.empty else Set.singleton v
        | Lam (p, x) -> halp (Set.add p bound) x
        | App (f, a) -> Set.union (halp bound f) (halp bound a)
        | ParamArray( OpList ops) -> List.fold(halp) bound ops 
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
        | Call _ -> Fine
        | Value v -> Fine
        | ParamArray(OpList ops) -> 
            ops 
            |> List.map (fun op -> op, halp op)
            |> List.fold (fun state (op,rop) -> 
                          match state,rop with
                          | Renamed(ParamArray(OpList ops)),_ -> Renamed(ParamArray(OpList (op::ops)))
                          | Renamed (_) as r,_ -> r
                          | Fine,Renamed rop -> Renamed(ParamArray(OpList [rop]))
                          | Fine,Fine -> Fine) Fine 
            |> function 
            | Renamed(ParamArray(OpList ops)) -> Renamed(ParamArray(OpList <| List.rev ops))
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
        | ParamArray(OpList ops) -> 
            let (rops,expr) = 
                ops 
                |> List.map (fun op -> match reduce op with 
                                                  | Normal -> op,Normal
                                                  | Next rop -> rop,Next rop)
                |> List.unzip                            
            if List.exists(function Next _ -> true | Normal -> false) expr then
                Next (ParamArray(OpList rops))
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
let rec evalOp evalCall env (operation:Operation) = 
      //let all = allIds operation
      match operation with
      
      | ParamArray(OpList ops) -> 
            ops
            |> List.fold(fun acc op -> let newOp = evalOp evalCall env op
                                       (newOp::acc)) [] 
            |> List.rev 
            |> OpList |> ParamArray 
      | Value _ as v -> v
      | Call _ as c -> c
      | Var (var)  -> Map.tryFind (var) env |> function Some v -> v | None -> printfn "Couldn't find value %s" var; noValue
      | Let(str, var, op) -> 
            let result = evalOp evalCall env var
            evalOp evalCall (Map.add str result env) op
      | Lam _ as l -> l
      | App(f, value) -> 
           match evalOp evalCall env f, evalOp evalCall env value with 
           | (Call f),v -> evalCall f v env 
           | _ -> failwith "Cannot apply to something not a function"
let tryFindLabel name operation = 
    None
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