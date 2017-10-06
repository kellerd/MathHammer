module Probability.View
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Check
open Distribution

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
      

