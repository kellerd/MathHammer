#r "../packages/Fable.Core/lib/netstandard1.6/Fable.Core.dll"
#r "../packages/Fable.Elmish/lib/netstandard1.6/Fable.Elmish.dll"
#r "../packages/Fable.Elmish.React/lib/netstandard1.6/Fable.Elmish.React.dll"
#r "../packages/Fable.Elmish.Browser/lib/netstandard1.6/Fable.Elmish.Browser.dll"
#r "../packages/Fable.React/lib/netstandard1.6/Fable.React.dll"
#load "../src/GameActions/Primitives/Types.fs"
#load "../src/GameActions/Primitives/View.fs"
#load "../src/GameActions/GameActionsList/Types.fs"
#load "../src/GameActions/GameActionsList/State.fs"
#load "../src/GameActions/GameActionsList/View.fs"
#load "../src/GameActions/Types.fs"
#load "../src/GameActions/State.fs"
#load "../src/GameActions/View.fs"
#load "../src/Maths/Probability.fs"
#load "../src/Maths/Result.fs"
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

open MathHammer.Models.State
open GameActions.Primitives.Types
open Result
let meq = initMeq "SpaceMarine" |> fst

open MathHammer.Models.View
let rangeStops rangeOperation = 
    let meleeRange = reduce rangeOperation
    let length = List.length meleeRange
    let minRange, maxRange,minProbability,maxProbability =
        meleeRange 
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
        else (1. - (float (range - minRange) / float(maxRange - minRange))) * 255. |> int  |>  sprintf "#77%02X00"
    let opacity prob = 
        if maxProbability - minProbability = 0.0 then 1.0
        else (prob - minProbability) / (maxProbability - minProbability) 
        |> sprintf "%.2f"
    let stopsPercentGreenAndOpacity = 
        meleeRange 
        |> List.mapi (fun i (range,prob) -> 
            match range,prob with 
            | Fail _,_ | _,0.0 -> stopPercent i length, "#FF0000", "0.0"
            | Pass range,_ -> stopPercent i length, percentGreen (inch.ToMM(int range * 1<inch>)), opacity prob
            | _ -> failwith "invalid range calculation")  
    minRange, maxRange,stopsPercentGreenAndOpacity
let rangeOperation = meq.ShootingRange
rangeStops meq.ShootingRange    

//   <defs>
//     <radialGradient id="exampleGradient">
//       <stop offset="10%" stop-color="gold"/>
//       <stop offset="95%" stop-color="green"/>
//     </radialGradient>
//   </defs>

//   <circle fill="url(#exampleGradient)" cx="60" cy="60" r="50"/>