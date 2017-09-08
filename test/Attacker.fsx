#load "LoadModules.fsx"
open GameActions.Primitives.Types
open MathHammer.Models.State
open GameActions.Primitives.State
open GameActions.Primitives.View

let ws = bindOp "WS" (get "WS")
let bs = bindOp "BS" (get "BS")
let x = (ws >> bs) allProps  |> lam "BS" |> lam "WS" 
let attcker = bindOp "WS" (vInt 6) (bindOp "BS" (vInt 3) allProps)
let attcker' = allProps |> lam "BS" |> lam "WS" 
vInt 3 |%> (vInt 6 |%> attcker') |> normalizeOp |> evalOp Map.empty<_,_>
let body = nestOps [hitResults;chargeRange;meleeRange;psychicTest] allProps
let defbody = nestOps [hitResults;shootingRange;psychicTest] allProps
let stats = ["M";"WS";"BS";"S";"T";"W";"A";"Ld";"Sv";"InvSv"] 
let attacker = createArgs stats body
let args = [vInt 8; dPlus D6 3 ;dPlus D6 3; vInt 4; vInt 4; vInt 1; vInt 2; vInt 8; dPlus D6 3; Value(NoValue)] 
let evaluated = applyArgs attacker args |> normalizeOp |> evalOp Map.empty<_,_>
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