module Probability.View
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Result
open Distribution

let rec passFailToExpectation = function Pass _ -> float 0xFF  | Fail _ -> float 0x00 | List xs -> List.averageBy passFailToExpectation xs | Tuple (x,y) -> if x + y = 0. then float 0xFF
                                                                                                                                                            else (x / (x + y) ) * float 0xFF
let normalize minX maxX low high x =
      (high - low) * ( (x - minX) / (maxX - minX) ) + low
let normalizeBy by mapping low high xs =
      let minX = List.minBy by xs |> by
      let maxX = List.maxBy by xs |> by
      let normal = normalize minX maxX low high
      List.map (fun x -> by x |> normal |> mapping x) xs

let opacity minProbability maxProbability prob = 
    if maxProbability - minProbability = 0.0 then 1.0
    else normalize minProbability maxProbability 0.5 1. prob
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
                        |> List.map (fun (r, prob) -> 
                              let greenValue = Result.map float r |> passFailToExpectation
                              //let percentageGreen = prob / max * 255.
                              let alpha = opacity min max prob
                              //let colour = sprintf "#%02X%02X00" (0xFF - System.Convert.ToInt32 percentageGreen) (System.Convert.ToInt32(percentageGreen))
                              div [Style [Color (colourA greenValue alpha)]] [str (printResultD r); str <| sprintf " %.2f%%" (prob / total)])
                    |> div [ClassName "column"]            
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column  is-narrow"] [str " => "]
            dist |> probabilities           
          ]

let showAverages (key, dist) = 
      let expectations dist = 
            let colour = 
                  dist 
                  |> expectation passFailToExpectation 
                  |> colour
            let result = dist |> List.fold (fun sum (v,probability) -> Result.mult v (Pass probability) |> Result.add sum ) (Fail 0.)
            // match result with 
            // | Pass x   -> printf "Pass %.2f" x 
            // | Fail x   -> printf "Fail %.2f" x     
            // | List _ -> printf "List"  
            // | Tuple(x,y) ->  printf "Pass %.2f" x 
            //      |> List.sumBy (fun (v,probability) -> Result.mult v (Pass probability))
            div [ClassName "column"; Style [Color colour]] 
                [ str <| printResultF result]
                //[str (printResultF result)]
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column"] [str " => "]
            dist |> Distribution.map(Result.map float) |> expectations           
          ]

let showSample (key, dist) = 
      let sampleDistribution dist = 
            let result = dist |> sample 
            let colour' = result  |> Result.map float |> passFailToExpectation  |> colour
            div [ClassName "column"; Style[Color colour']] [printResultD result |> str ]
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column"] [str " => "]
            dist |> sampleDistribution           
          ]  