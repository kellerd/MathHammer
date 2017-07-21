#r "../packages/Fable.Core/lib/netstandard1.6/Fable.Core.dll"
#r "../packages/Fable.Elmish/lib/netstandard1.6/Fable.Elmish.dll"
#r "../packages/Fable.Elmish.React/lib/netstandard1.6/Fable.Elmish.React.dll"
#r "../packages/Fable.Elmish.Browser/lib/netstandard1.6/Fable.Elmish.Browser.dll"
#r "../packages/Fable.React/lib/netstandard1.6/Fable.React.dll"
#load "../src/Result/Result.fs"
#load "../src/Probability/Distribution.fs"
#load "../src/Probability/Determinism.fs"
#load "../src/Probability/View.fs"
#load "../src/GameActions/Primitives/Types.fs"
#load "../src/GameActions/Primitives/State.fs"
#load "../src/GameActions/Primitives/View.fs"
#load "../src/GameActions/GameActionsList/Types.fs"
#load "../src/GameActions/GameActionsList/State.fs"
#load "../src/GameActions/GameActionsList/View.fs"
#load "../src/GameActions/Types.fs"
#load "../src/GameActions/State.fs"
#load "../src/GameActions/View.fs"
#load "../src/MathHammer/Models/Types.fs"
#load "../src/MathHammer/Models/State.fs"

open GameActions.Primitives.Types
open MathHammer.Models.State
open GameActions.Primitives.View


//zero = fun f -> fun x -> x
let zero = Lam("f",Lam("x", Var("x")))
normalizeOp zero
normalizeOp (App(zero,(Var("g"))))
let one = Lam("f",Lam("x", App(Var("f"),Var("x"))))
let two = Lam( "f", Lam("x",App(Var("f"),App(Var("f"),Var("x")))))

let (|%>) x f = App(f,x)

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
let count ops = App(Call Count,Value(ManyOp(OpList ops)))
let count' ops = App(Call Count,Value(ManyOp(Unfold ops)))

let normalizedFirst = count [normalizeOp appliedTwo;normalizeOp appliedTwo;normalizeOp appliedTwo] |> normalizeOp
let normalizedSecond = count [appliedTwo;appliedTwo;appliedTwo] |> normalizeOp

normalizedFirst = normalizedSecond

let normalizedFirst' = count' (normalizeOp appliedTwo,normalizeOp appliedTwo) |> normalizeOp
let normalizedSecond' = count'  (appliedTwo,appliedTwo) |> normalizeOp

normalizedFirst' = normalizedSecond'

let v = Value(Int(3))
let retValueIsSame v f =
    let evaled = Let("x", v ,Var ("x")) |> f |> evalOp Map.empty<_,_> |> snd
    let evaled' = v |> f |> evalOp Map.empty<_,_> |> snd
    evaled = evaled'
retValueIsSame v id
retValueIsSame v normalizeOp

let addition x y  =    
    let v = Value(Int(x))
    let (Value(Dist(d))) =
        Let("x", v ,App(Call Total, Value(ManyOp(OpList[Value(Int(y));Var ("x")])))) 
        |> evalOp Map.empty<_,_>
        |> snd
    let expected = Distribution.always (Result.Pass (x + y))
    printf "Is %A = %A" d expected
    d = expected
addition 3 9    

let totalOfXIsX x = 
    let v = Value(Int(x))
    let (Value(Dist(d))) =
        Let("x", v ,App(Call Total, v)) 
        |> evalOp Map.empty<_,_>
        |> snd
    let expected = Distribution.always (Result.Pass (x))
    printf "Is %A = %A" d expected
    d = expected
totalOfXIsX 6  

let countOfOneXIsOneX x = 
    let v = Value(Int(x))
    let (Value(Dist(d))) =
        Let("x", v ,App(Call Count, v)) 
        |> evalOp Map.empty<_,_>
        |> snd
    let expected = Distribution.always (Result.Tuple (1,0))
    printf "Is %A = %A" d expected
    d = expected
countOfOneXIsOneX 6  