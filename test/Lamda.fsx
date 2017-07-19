#load "LoadModules.fsx"

open GameActions.Primitives.Types
open MathHammer.Models.State
open GameActions.Primitives.View

let rec runEval x = 
      System.Console.WriteLine (unparse x)
      match reduce'' x with 
      | Normal -> ()
      | Next nx -> runEval nx

//zero = fun f -> fun x -> x
let zero = Lam(Global,"f",Lam(Global,"x", Var(Global,"x")))
runEval zero
runEval (App(zero,(Var(Global,"g"))))
let one = Lam(Global,"f",Lam(Global,"x", App(Var(Global,"f"),Var(Global,"x"))))
let two = Lam(Global, "f", Lam(Global,"x",App(Var(Global,"f"),App(Var(Global,"f"),Var(Global,"x")))))

let (|%>) x f = App(f,x)

runEval one
Var(Global,"h") |%> (Var(Global,"g") |%> zero) |> runEval
Var(Global,"h") |%> (Var(Global,"g") |%> one) |> runEval
Var(Global,"h") |%> (Var(Global,"g") |%> two) |> runEval
runEval (App(one,(Var(Global,"g"))))
reduce'' (App(App(one,(Var(Global,"g"))),Var(Global,"7")))
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