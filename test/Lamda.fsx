#load "LoadModules.fsx"

open GameActions.Primitives.Types
open MathHammer.Models.State
open GameActions.Primitives.State
open GameActions.Primitives.View


//zero = fun f -> fun x -> x
let zero = Lam("f",Lam("x", Var("x")))
normalizeOp zero
normalizeOp (App(zero,(Var("g"))))
let one = Lam("f",Lam("x", App(Var("f"),Var("x"))))
let two = Lam( "f", Lam("x",App(Var("f"),App(Var("f"),Var("x")))))


normalizeOp one
Var("h") |%> (Var("g") |%> zero) |> normalizeOp
Var("h") |%> (Var("g") |%> one) |> normalizeOp
Var("h") |%> (Var("g") |%> two) |> normalizeOp
normalizeOp (App(one,(Var("g"))))
normalizeOp (App(App(one,(Var("g"))),Var("7")))
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
Var("h") |%> (Var("g") |%> zero) |> normalizeOp |> unparse
Var("h") |%> (Var("g") |%> one)  |> normalizeOp  |> unparse
Var("h") |%> (Var("g") |%> two)  |> normalizeOp  |> unparse

let appliedTwo = Var("h") |%> (Var("g") |%> two)
let count ops = App(Call Count,ParamArray(OpList ops))
let count' ops = App(Call Count,App(Call Unfold, ParamArray(OpList ops)))

let normalizedFirst = count [normalizeOp appliedTwo;normalizeOp appliedTwo;normalizeOp appliedTwo] |> normalizeOp
let normalizedSecond = count [appliedTwo;appliedTwo;appliedTwo] |> normalizeOp

normalizedFirst = normalizedSecond

let normalizedFirst' = count' [normalizeOp appliedTwo;normalizeOp appliedTwo;normalizeOp appliedTwo]|> normalizeOp
let normalizedSecond' = count'  [appliedTwo;appliedTwo;appliedTwo]  |> normalizeOp

normalizedFirst' = normalizedSecond'

let v = Value(Int(3))
//Let x = 3 returns 3 as well as binding to environment
let retValueIsSame v f =
    let evaled = Let("x", v ,Var ("x")) |> f |> evalOp standardCall Map.empty<_,_> 
    let evaled' = v |> f |> evalOp standardCall Map.empty<_,_> 
    evaled = evaled'
retValueIsSame v id
retValueIsSame v normalizeOp
//Let x = some number in
//x + some other number
let addition x y  =    
    let v = Value(Int(x))
    let (Value(Dist(d))) =
        Let("x", v ,App(Call Total, ParamArray(OpList[Value(Int(y));Var ("x")])))
        |> evalOp standardCall Map.empty<_,_>
    let expected = Distribution.always (Int(x + y))
    printfn "Is %A = %A" d expected
    d = expected
//Let x = 6
//Total of x is 6
let totalOfXIsX x = 
    let v = Value(Int(x))
    let (Value(Dist(d))) =
        Let("x", v ,App(Call Total, v)) 
        |> evalOp standardCall Map.empty<_,_>
    let expected = Distribution.always (Int(x))
    printfn "Is %A = %A" d expected
    d = expected
//Count of one passed result is 1
let countOfOneXIsOneX x = 
    let v = Value(Result(Result.Pass(Int(x))))
    let (Value(Dist(d))) =
        Let("x", v ,App(Call Count, v)) 
        |> evalOp standardCall Map.empty<_,_>
    let expected = Distribution.always (Result(Result.Tuple (Int(1),Int(0))))
    printfn "Is %A = %A" d expected
    d = expected

totalOfXIsX 6  
addition 3 9    
countOfOneXIsOneX 6  

let unfoldD6 = 
    let WS = dPlus D6 3
    let A = vInt 3
    let unfold = unfoldOp WS A
    unfold 
    |> evalOp standardCall Map.empty<_,_>

