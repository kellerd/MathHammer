module MathHammer.Models.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Fable.Import
open GameActions.Primitives.Types
open Result
open Probability
let onClick x : IProp = OnClick(x) :> _
let list d = List d 
let tuple d = Tuple d
let getResult = function Pass f | Fail f -> Some f | _ -> None



let showProbabilitiesOfActions env (key, operation) = 
      let probabilities (dist:Distribution<_>) = 
            let result = 
                  dist 
                  |> List.groupBy fst
                  |> List.map(fun (f,probs) -> f, List.sumBy snd probs)
            match result with 
            | [] -> str ""
            | _ ->  let total = result |> List.maxBy snd |> snd
                    result
                        |> List.map (fun (f, prob) -> 
                              let proabilityGreen = prob / total * 255.
                              div [Style [Color (proabilityGreen |> int |> sprintf "#77%02X00")]] [str (printResult f)])
                    |> div [ClassName "column"]            
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column  is-narrow"] [str " => "]
            reduce env operation |> snd |> probabilities           
          ]
let showAverages env (key, operation) = 
      let expectations (dist:Distribution<_>) = 
            let colour = 
                  dist 
                  |> expectation (function Pass _ -> float 0x00FF00  | Fail _ -> float 0xFF0000 | Tuple _ | List _-> float 0x000000) 
                  |> int
                  |> sprintf "#%06X" 
            let result = dist |> List.map fst |> resultListAverage
            div [ClassName "column"; Style [Color colour]] [str (printResult result)]
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column"] [str " => "]
            reduce env operation |> snd |> expectations           
          ]
let showSample env (key, operation) = 
      let sampleDistribution (dist:Distribution<_>) = 
            let result = dist |> sample 
            let colour = (match result with Pass _ -> float 0x00FF00  | Fail _ -> float 0xFF0000 | Tuple _ | List _-> float 0x000000) |> int
            let colour' = sprintf "#%06X" colour
            div [ClassName "column"; Style[Color colour']] [printResult result |> str ]
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column"] [str " => "]
            reduce env operation |> snd |> sampleDistribution           
          ]
let rec printDs d  =
      match d with 
      | D3 -> "D3"
      | D6 -> "D6"
      | Reroll (rerolls, d') -> sprintf "Reroll (%s) in %s" (List.distinct rerolls |> List.map string |> String.concat ";") (printDs d')
let printPrimitive p = 
      match p with
      | Int i -> sprintf "%d" i
      | Dice d -> printDs d 

let showActions dispatch (key, operation)  = 
  
  div []
      [ b [] [str key; str " : "]
        GameActions.Primitives.View.root operation dispatch  ]

let showAttributes (key, operation) dispatch = 
  
  div [ClassName "has-text-centered column"]
      [ b  [] [str key]
        br []
        GameActions.Primitives.View.root operation dispatch ]

let rangeStops rangeOperation env = 
    let (env',meleeRange) = reduce env rangeOperation
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
    env,
    (minRange,maxRange, 
     stopsPercentGreenAndOpacity  
     |> List.map(fun (offset,stopcolor,opacity) -> 
                        stop [ Offset !^ offset
                               StopColor stopcolor
                               StopOpacity !^ opacity ] []))

let groupFor model display = 
      g     [Transform <| sprintf "translate(%f,%f)" model.PosX model.PosY]
            [ g   [ Transform model.Scale ] display ]

let rangeRoot env model dispatch =
      let ranges id (min:int<mm>,max:int<mm>,stops) = 
            g [] 
              [   defs  [] 
                        [ radialGradient [ Id id ]
                                           stops ]
                  circle [Fill <| sprintf "url(#%s)" id
                          R !^ (float max)] [] ]
      [ "meleeRanges",rangeStops model.MeleeRange 
        "shootingRange",rangeStops model.ShootingRange ]                          
      |> List.fold (fun (env,acc) (name,f) -> 
            let (newEnv,rangeStops) = f env

            
            (newEnv,ranges name rangeStops::acc)) (env,[])  
      |> snd |> groupFor model 
let root model dispatch =
      let modelDisplay = 
            [ circle   [ R !^ (model.Size / 2 |> float) :> IProp
                         OnClick (fun mouseEvt -> Select |> dispatch) :> IProp] []
              text     [ TextAnchor "middle"
                         Y !^ 50.
                         StrokeWidth (!^ ".5")
                         Fill "#000000"
                         Stroke "#000000"
                         FontSize !^ "25"    ] 
                         [ str model.Name ] ]
            //
            
            
      groupFor model modelDisplay

