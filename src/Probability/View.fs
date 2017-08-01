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
    else normalize minProbability maxProbability 0.6 1. prob
let colourA (greenValue:float) alpha = sprintf "rgba(%d,%d,0,%f)" (0xFF - System.Convert.ToInt32 greenValue) (System.Convert.ToInt32(greenValue)) alpha
let colour (greenValue:float) = sprintf "#%02X%02X00" (0xFF - System.Convert.ToInt32 greenValue) (System.Convert.ToInt32(greenValue)) 



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
            dist |> Distribution.map(Result.map float) |> expectations           
          ]

