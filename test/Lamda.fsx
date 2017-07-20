#load "LoadModules.fsx"

open GameActions.Primitives.Types
open MathHammer.Models.State
open GameActions.Primitives.View


//zero = fun f -> fun x -> x
let zero = Lam(Global,"f",Lam(Global,"x", Var(Global,"x")))
normalizeOp zero
normalizeOp (App(zero,(Var(Global,"g"))))
let one = Lam(Global,"f",Lam(Global,"x", App(Var(Global,"f"),Var(Global,"x"))))
let two = Lam(Global, "f", Lam(Global,"x",App(Var(Global,"f"),App(Var(Global,"f"),Var(Global,"x")))))

let (|%>) x f = App(f,x)

normalizeOp one
Var(Global,"h") |%> (Var(Global,"g") |%> zero) |> normalizeOp
Var(Global,"h") |%> (Var(Global,"g") |%> one) |> normalizeOp
Var(Global,"h") |%> (Var(Global,"g") |%> two) |> normalizeOp
normalizeOp (App(one,(Var(Global,"g"))))
normalizeOp (App(App(one,(Var(Global,"g"))),Var(Global,"7")))
allIds one

<@let zero' (f:int->int) (x:int) = x
zero' ((+) 1) 0@>
<@let one' (f:int->int) (x:int) = f x 
one' ((+) 1) 0 @>
<@let two' (f:int->int) (x:int) = f (f x) 
two' ((+) 1) 0 @>

freeIds zero
freeIds one
freeIds two
zero |> normalizeOp |> unparse
one |> normalizeOp |> unparse
two |> normalizeOp |> unparse
Var(Global,"h") |%> (Var(Global,"g") |%> zero) |> normalizeOp |> unparse
Var(Global,"h") |%> (Var(Global,"g") |%> one)  |> normalizeOp  |> unparse
Var(Global,"h") |%> (Var(Global,"g") |%> two)  |> normalizeOp  |> unparse

let appliedTwo = Var(Global,"h") |%> (Var(Global,"g") |%> two)
let count ops = Call(Count,OpList ops)
let count' ops = Call (Count, Unfold ops)

let normalizedFirst = count [normalizeOp appliedTwo;normalizeOp appliedTwo;normalizeOp appliedTwo] |> normalizeOp
let normalizedSecond = count [appliedTwo;appliedTwo;appliedTwo] |> normalizeOp

normalizedFirst = normalizedSecond

let normalizedFirst' = count' (normalizeOp appliedTwo,normalizeOp appliedTwo) |> normalizeOp
let normalizedSecond' = count'  (appliedTwo,appliedTwo) |> normalizeOp

normalizedFirst' = normalizedSecond'

