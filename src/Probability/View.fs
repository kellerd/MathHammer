module Probability.View
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Result
open Distribution

let rec passFailToExpectation = function Pass _ -> float 0xFF  | Fail _ -> float 0x00 | List xs -> List.averageBy passFailToExpectation xs | Tuple _ -> float 0xFF
let normalize low high minX maxX x =
      (high - low) * ( (x - minX) / (maxX - minX) ) + low
let opacity minProbability maxProbability prob = 
    if maxProbability - minProbability = 0.0 then 1.0
    else normalize 0.5 1. minProbability maxProbability prob
let colourA (greenValue:float) alpha = sprintf "rgba(%d,%d,0,%f)" (0xFF - System.Convert.ToInt32 greenValue) (System.Convert.ToInt32(greenValue)) alpha
let colour (greenValue:float) = sprintf "#%02X%02X00" (0xFF - System.Convert.ToInt32 greenValue) (System.Convert.ToInt32(greenValue)) 

let showProbabilitiesOfActions (key,dist) = 
      let probabilities (dist:Distribution<_>) = 
            let result = 
                  dist 
                  |> List.groupBy fst
                  |> List.map(fun (f,probs) -> f, List.sumBy snd probs)
            match result with 
            | [] -> str ""
            | _ ->  let max = result |> List.maxBy snd |> snd
                    let min = result |> List.minBy snd |> snd
                    let total = result |> List.sumBy snd 
                    result
                        |> List.map (fun (f, prob) -> 
                              let greenValue = passFailToExpectation f
                              //let percentageGreen = prob / max * 255.
                              let alpha = opacity min max prob
                              //let colour = sprintf "#%02X%02X00" (0xFF - System.Convert.ToInt32 percentageGreen) (System.Convert.ToInt32(percentageGreen))
                              div [Style [Color (colourA greenValue alpha)]] [str (printResult f); str <| sprintf " %.2f%%" (prob / total)])
                    |> div [ClassName "column"]            
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column  is-narrow"] [str " => "]
            dist |> probabilities           
          ]

let showAverages (key, dist) = 
      let expectations (dist:Distribution<_>) = 
            let colour = 
                  dist 
                  |> expectation passFailToExpectation 
                  |> colour
            let result = dist |> List.sumBy (fun (v,probability) -> v * probability)
            div [ClassName "column"; Style [Color colour]] [str (printResult result)]
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column"] [str " => "]
            dist |> expectations           
          ]

let showSample (key, dist) = 
      let sampleDistribution (dist:Distribution<_>) = 
            let result = dist |> sample 
            let colour' = passFailToExpectation result |> colour
            div [ClassName "column"; Style[Color colour']] [printResult result |> str ]
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column"] [str " => "]
            dist |> sampleDistribution           
          ]  