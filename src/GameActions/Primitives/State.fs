module GameActions.Primitives.State

open Types
let get v = Var (v)
let single op = Value(ManyOp(List.singleton op |> OpList))
let opList ops = Value(ManyOp(OpList ops))
let unfoldOp op op2 =  Value(ManyOp(Unfold(op,op2)))
let call f op = App(Call f, op)
let count v = v |> call Count
let total v = v |> call Total
let product v = v |> call Product
let bindVal text op = Let(text,op,Var text)

let bindOp v op inBody = Operation.Let(v,op, inBody)
let lam s op = Lam (s,op)
let (|~>) o v = bindVal v o
let (>>=) o v = bindOp v o

let d6 = Value(Dice(D6))

let dPlus d v = Value(DPlus(d,v))
let vInt i = Value(Int(i))
let emptyOp = Value(NoValue)
let hitResults = 
    get "WS" |> single |> total
    |~> "HitResults"

let chargeRange =
    [d6;d6] |> opList |> total
    >>= "ChargeRange"

let meleeRange = 
    [ get "M" 
      get "Attacker" |> chargeRange] 
    |> opList 
    |> total 
    |~>  "MeleeRange"

let psychicTest = [d6;d6] |> opList |> total |~> "PsychicTest"

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