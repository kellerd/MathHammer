#load "LoadModules.fsx"
open GameActions.Primitives.Types
open MathHammer.Models.State
open GameActions.Primitives.State
open GameActions.Primitives.View
let allProps = 
    opList 
        [ labelVar "M"
          labelVar "WS"
          labelVar "BS"
          labelVar "S"
          labelVar "T"
          labelVar "W"
          labelVar "A"
          labelVar "Sv"
          labelVar "InvSv"
          labelVar "MeleeRange"
          labelVar "ShootingRange"
          labelVar "PsychicTest"
          labelVar "HitResults"
          labelVar "ChargeRange" ]

let ws = bindOp "WS" (get "WS")
let bs = bindOp "BS" (get "BS")
let x = (ws >> bs) allProps  |> lam "BS" |> lam "WS" 
let attcker = bindOp "WS" (vInt 6) (bindOp "BS" (vInt 3) allProps)
let attcker' = allProps |> lam "BS" |> lam "WS" 
vInt 3 |%> (vInt 6 |%> attcker') |> normalizeOp |> evalOp Map.empty<_,_>

let hitResults = get "WS" |> single |> total >>= "HitResults"
let chargeRange = [d6;d6] |> opList |> total >>= "ChargeRange"
let meleeRange = opList [ get "M"; get "ChargeRange" ] |> total >>= "MeleeRange"
let psychicTest = [d6;d6] |> opList |> total >>= "PsychicTest"
let stats = ["M";" WS";"BS";"S";"T";"W";"A";"Sv";"InvSv"]
let attackerTests = [hitResults;chargeRange;meleeRange;psychicTest] |> List.rev |> List.reduce (>>)
let attackerReturning = attackerTests allProps
let attacker = stats |> List.rev |> List.fold (fun f arg -> lam arg f) attackerReturning

let args = [vInt 8; dPlus D6 3 ;dPlus D6 3; vInt 4; vInt 4; vInt 1; vInt 2; dPlus D6 3; Value(NoValue)] 

let applied = 
    attacker::args
    |> List.reduce apply
    |> normalizeOp

let evaluated = applied |> evalOp Map.empty<_,_>

let attackerExpanded = 
    Let("M",vInt 6, 
        Let("MeleeRange", 
            Let("Total", App(Call Total, Value(ManyOp(OpList[get "M"; vInt 12]))), 
                Var "Total"), allProps))
attacker |> normalizeOp |> evalOp Map.empty<_,_>

let attackerQuote = 
    <@ 
        let m = 6
        let a = 5
        ["M",m; "A",a] @>