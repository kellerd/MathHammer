module GameActions.Primitives.State

open Types
let get v = Var (v)
let noValue = NoValue |> Value
let single op = Value(ManyOp(List.singleton op |> OpList))
let opList ops = Value(ManyOp(OpList ops))
let pair x y = opList [x;y]
let call f op = App(Call f, op)
let count v = v |> call Count
let unfoldOp op op2 = opList [op; op2] |> call Unfold
let total v = v |> call Total
let product v = v |> call Product
let bindVal text op = Let(text,op,Var text)

let bindOp v op inBody = Operation.Let(v,op, inBody)
let lam s op = Lam (s,op)

let d6 = Value(Dice(D6))
let str s = Str s |> Value

let dPlus d v = [Value(Dice(d));Value(Int(v))] |> opList |> call GreaterThan
let vInt i = Value(Int(i))
let emptyOp = Value(NoValue)
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
          labelVar "MeleeRange"
          labelVar "ShootingRange"
          labelVar "PsychicTest"
          labelVar "HitResults"
          labelVar "ChargeRange" ]

let hitResults = get "WS" |> single |> total >>= "HitResults"
let chargeRange = [d6;d6] |> opList |> total >>= "ChargeRange"
let meleeRange = opList [ get "M"; get "ChargeRange" ] |> total >>= "MeleeRange"
let shootingRange = opList [get "WeaponRange"] >>= "ShootingRange"
let psychicTest = [d6;d6] |> opList |> total >>= "PsychicTest"

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