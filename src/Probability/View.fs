module Probability.View
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Result
open Distribution
let showProbabilitiesOfActions (key,dist) = 
      let probabilities (dist:Distribution<_>) = 
            let result = 
                  dist 
                  |> List.groupBy fst
                  |> List.map(fun (f,probs) -> f, List.sumBy snd probs)
            match result with 
            | [] -> str ""
            | _ ->  let max = result |> List.maxBy snd |> snd
                    let total = result |> List.sumBy snd 
                    result
                        |> List.map (fun (f, prob) -> 
                              let percentageGreen = prob / max 
                              div [Style [Color (percentageGreen * 255. |> int |> sprintf "#77%02X00")]] [str (printResult f); str <| sprintf " %.2f%%" (prob / total)])
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
                  |> expectation (function Pass _ -> float 0x00FF00  | Fail _ -> float 0xFF0000 | Tuple _ | List _-> float 0x000000) 
                  |> int
                  |> sprintf "#%06X" 
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
            let colour = (match result with Pass _ -> float 0x00FF00  | Fail _ -> float 0xFF0000 | Tuple _ | List _-> float 0x000000) |> int
            let colour' = sprintf "#%06X" colour
            div [ClassName "column"; Style[Color colour']] [printResult result |> str ]
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column"] [str " => "]
            dist |> sampleDistribution           
          ]  