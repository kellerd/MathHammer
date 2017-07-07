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
open Probability.View
let hitMelee = 
    Let(Attacker, "Melee", Let(Attacker, "MeleeHits", Count[Multiply(Var(Attacker, "WS"), Var(Attacker, "A"))]))
let A = Let(Attacker, "A", Value(Int(1)))
let WS = Let(Attacker, "WS", DPlus(D6,3))
let Psychic = Let(Attacker, "Psychic", Total[Value(Dice(D6));Value(Dice(D6))])
let result = reduce Map.empty<_,_> Psychic |> snd

let env = 
    [A;WS;hitMelee]
    |> List.fold (fun env -> reduce env >> fst) Map.empty<_,_>


let dist = reduce env Psychic |> snd
let rangeStops dist = 
    let length = List.length dist
    let minRange, maxRange,minProbability,maxProbability =
        dist 
        |> List.fold (fun (currMinRange,currMaxRange,currMin,currMax) (range,prob) -> 
            min currMinRange range, max currMaxRange range,
            min currMin prob, max currMax prob ) (Pass (28.<ft> * 12.<inch/ft> |> float),Pass 0.,1.,0.)
        |> function 
           | (Pass minRange, Pass maxRange,minProbability,maxProbability) ->
                inch.ToMM(int minRange * 1<inch>), inch.ToMM(int maxRange * 1<inch>),minProbability,maxProbability
           | (Fail _, Pass maxRange,minProbability,maxProbability) ->  
                0<mm>, inch.ToMM(int maxRange * 1<inch>),minProbability,maxProbability  
           | (_, _,minProbability,maxProbability) ->  
                0<mm>, 0<mm>,0.,0.       
    let stopPercent i length = float (i + 1) / float length |> sprintf "%.2f"
    let percentGreen (range:int<mm>) =
        if maxRange - minRange = 0<mm> then "#00FF00"
        else colour <| (1. - (float (range - minRange) / float(maxRange - minRange))) * 255. 


    let stopsPercentGreenAndOpacity = 
        let stops = 
            dist 
            |> List.toArray
            |> Array.mapi (fun i (range,prob) -> 
                match range,prob with 
                | Fail _,_ | _,0.0 -> (stopPercent i length, colour 255.), 0.0
                | Pass range,_ -> (stopPercent i length, percentGreen (inch.ToMM(int range * 1<inch>))), prob
                | _ -> failwith "invalid range calculation") 
            |> Array.rev
        stops
        |> Array.scan (fun ((_,_),lastProb) ((stopPercent,green),prob) -> ((stopPercent,green),prob+lastProb)) (("0.00","#000000"), 0.0)                          
        |> List.ofArray 
        |> List.skip 1
        |> List.rev          
        |> Distribution.Operations.countedCases 
    (minRange,maxRange, stopsPercentGreenAndOpacity)
     
rangeStops dist

let inp = [ (("0.18", "#19E600"), 0.2);
            (("0.27", "#33CC00"), 0.4); (("0.36", "#4DB200"), 0.6);
            (("0.45", "#669900"), 0.8); (("0.55", "#7F8000"), 1.0);
            (("0.64", "#996600"), 0.8); (("0.73", "#B24D00"), 0.6);
            (("0.82", "#CC3300"), 0.4); (("0.91", "#E61900"), 0.2);
            (("1.00", "#FF0000"), 0.0)]


let total = Seq.sumBy (fun (_, v) -> v) inp
weightedCases (inp |> List.map (fun (x, v) -> (x, v / total)))    


let rec coinFlips w l =
    match l with
    | [] -> failwith "no coinFlips"
    | [(d, _)] -> always d
    | (d, p) :: rest -> coinFlip (p / (1.0 - w)) (always d) (coinFlips (w + p) rest)
coinFlips 0.0 inp