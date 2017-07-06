#r "../packages/Fable.Core/lib/netstandard1.6/Fable.Core.dll"
#r "../packages/Fable.Elmish/lib/netstandard1.6/Fable.Elmish.dll"
#r "../packages/Fable.Elmish.React/lib/netstandard1.6/Fable.Elmish.React.dll"
#r "../packages/Fable.Elmish.Browser/lib/netstandard1.6/Fable.Elmish.Browser.dll"
#r "../packages/Fable.React/lib/netstandard1.6/Fable.React.dll"
#load "../src/Result/Result.fs"
#load "../src/Probability/Distribution.fs"
#load "../src/Probability/View.fs"
#load "../src/GameActions/Primitives/Types.fs"
#load "../src/GameActions/Primitives/View.fs"
#load "../src/GameActions/GameActionsList/Types.fs"
#load "../src/GameActions/GameActionsList/State.fs"
#load "../src/GameActions/GameActionsList/View.fs"
#load "../src/GameActions/Types.fs"
#load "../src/GameActions/State.fs"
#load "../src/GameActions/View.fs"
#load "../src/MathHammer/Models/Types.fs"
#load "../src/MathHammer/Models/State.fs"
#load "../src/MathHammer/Models/View.fs"
#load "../src/MathHammer/UnitList/Types.fs"
#load "../src/MathHammer/UnitList/State.fs"
#load "../src/MathHammer/UnitList/View.fs"
#load "../src/MathHammer/Types.fs"
#load "../src/MathHammer/State.fs"
#load "../src/MathHammer/View.fs"
#load "../src/Global.fs"
#load "../src/Types.fs"
#load "../src/State.fs"


open App.State
open Probability
open Result
open GameActions.Primitives.Types
open MathHammer.State
let hitMelee = 
    Let(Attacker, "Melee", Let(Attacker, "MeleeHits", Count[Multiply(Var(Attacker, "WS"), Var(Attacker, "A"))]))
let A = Let(Attacker, "A", Value(Int(1)))
let WS = Let(Attacker, "WS", DPlus(D6,3))
let env = 
    [A;WS;hitMelee]
    |> List.fold (fun env -> reduce env >> fst) Map.empty<_,_>