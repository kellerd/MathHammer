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
vInt 3 |%> (vInt 6 |%> attcker') |> normalizeOp |> evalOp sampleCall Map.empty<_,_>
vInt 3 |%> (vInt 6 |%> attcker') |> normalizeOp |> evalOp avgCall Map.empty<_,_>
vInt 3 |%> (vInt 6 |%> attcker') |> normalizeOp |> evalOp standardCall Map.empty<_,_>
let body = nestOps [hitResults;chargeRange;meleeRange;psychicTest] allProps
let defbody = nestOps [hitResults;shootingRange;psychicTest] allProps
let stats = ["M";"WS";"BS";"S";"T";"W";"A";"Ld";"Sv";"InvSv"] 
let attacker = createArgs stats body
let defender =  createArgs stats defbody
let args = [vInt 8; dPlus D6 3 ;dPlus D6 3; vInt 4; vInt 4; vInt 1; vInt 2; vInt 8; dPlus D6 3; Value(NoValue)] 
applyArgs attacker args |> normalizeOp |> evalOp sampleCall Map.empty<_,_>
applyArgs attacker args |> normalizeOp |> evalOp avgCall Map.empty<_,_>
applyArgs attacker args |> normalizeOp |> evalOp standardCall Map.empty<_,_>
let attackerExpanded = 
    Let("M",vInt 6, 
        Let("MeleeRange", 
            Let("Total", App(Call Total, ParamArray([get "M"; vInt 12])), 
                Var "Total"), allProps))

attackerExpanded |> normalizeOp |> evalOp sampleCall Map.empty<_,_>
attackerExpanded |> normalizeOp |> evalOp avgCall Map.empty<_,_>
attackerExpanded |> normalizeOp |> evalOp standardCall Map.empty<_,_>


attacker |> normalizeOp |> evalOp sampleCall Map.empty<_,_>
attacker |> normalizeOp |> evalOp avgCall Map.empty<_,_>
attacker |> normalizeOp |> evalOp standardCall Map.empty<_,_>

let attackerQuote = 
    <@ 
        let m = 6
        let a = 5
        ["M",m; "A",a] @>

(initMeq "Marine" attacker |> fst).Rules
|> normalizeOp
|> evalOp standardCall Map.empty<_,_>


(initMeq "Marine" attacker |> fst).Rules
|> pget "HitResults" 
|> normalizeOp
|> evalOp standardCall Map.empty<_,_>

(initGeq "Q" defender |> fst).Rules
|> normalizeOp
|> evalOp standardCall Map.empty<_,_>

