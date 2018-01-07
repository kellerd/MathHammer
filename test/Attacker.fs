module AttackerTests
open GameActions.Primitives.Types
open MathHammer.Models.State
open GameActions.Primitives.State
open Expecto
let (==?) x y = Expect.equal x y ""
let (==~) x y = 
    match y with 
    | Value(Dist(y)) -> Expect.contains (y |> List.map (fst >> Value)) x ""
    | _ -> Expect.equal x y ""
let body = nestOps [hitResults;chargeRange;meleeRange;psychicTest;woundResults] allProps
let defbody = nestOps [hitResults;shootingRange;psychicTest] allProps
let stats = ["M";"WS";"BS";"S";"T";"W";"A";"Ld";"Sv";"InvSv"] 
let attacker = createArgs stats body
let defender =  createArgs stats defbody
let move = vInt 6
let threePlus = dPlus D6 3
let ws = threePlus
let bs = threePlus
let s = vInt 4
let t = vInt 4
let w = vInt 1
let a = vInt 2
let ld = vInt 8
let sv = threePlus
let invSave = noValue
let seargent = [move;ws;bs;s;t;w;a;ld;sv;invSave;] 
let defApplied = applyArgs defender seargent |> normalize|> evalOp standardCall Map.empty<string,Operation>
let initialMap = Map.add "Defender" defApplied Map.empty<string,Operation>

let attApplied = applyArgs attacker seargent |> normalize

let eps x op = getp x op |> evalOp standardCall initialMap
//let epa x op = getp x op |> evalOp avgCall initialMap
let ep x op = getp x op |> evalOp sampleCall initialMap
let (|IsDPlus|_|) = function
    | Let (roll,App (Call (Dice D6),Value NoValue),
           Let (gt, App (Call GreaterThan,Value (ParamArray [Var roll'; Value (Int d)])),
                Let (eq, App (Call Equals,Value (ParamArray [Var roll''; Value (Int d')])),
                     App (Call Or,Value (ParamArray [Var eq'; Var gt']))))) 
        when roll = roll' && roll' = roll'' &&
             gt = gt' &&
             eq = eq' &&
             d = d'
             -> Some d
    | _ -> None
[<Tests>]
let tests = 
    let pairs = List.zip stats seargent 
    let expectedStd = 
        pairs        
        |> List.map(function 
                    | key,IsDPlus plus -> key,List.init 6 ((+) 1 >> fun i -> if i >= plus then Check(Check.Pass(Int(i))) else Check(Check.Fail(Int(i)))) |> Distribution.uniformDistribution |> List.rev |> Dist |> Value
                    | key,op -> key,op)
    // let expectedAvg = 
    //     pairs
    //     |> List.map(function 
    //                 | key,IsDPlus plus -> 
    //                     if 3.5 >= float plus then key,Value (Check (Check.Pass (Float 3.5)))
    //                     else key,Value (Check (Check.Fail (Float 3.5)))
    //                 | key,op -> 
    //                     key,op)               

    testList "Attacker Tests" [
        expectedStd 
        |> List.map(fun (key,expected) -> test (sprintf "Check %s" key) { eps key attApplied ==? expected })  
        |> testList "Std eval Tests";

        expectedStd 
        |> List.map(fun (key,expected) -> test (sprintf "Check %s" key) { ep key attApplied ==~ expected })  
        |> testList "Sample Tests";
        
        // expectedAvg
        // |> List.map(fun (key,expected) -> test (sprintf "Check %s" key)  { epa key attApplied ==? expected })  
        // |> testList "Avg Tests";

    ] 


