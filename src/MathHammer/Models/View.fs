module MathHammer.Models.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Fable.Import
open Probability

let onClick x : IProp = OnClick(x) :> _


type Result = Pass of float | Fail of float 
let pass i = float i |> Pass
let fail i = float i  |> Fail
let getResult = function Pass f | Fail f -> f
let dPlus plus die = dist {
      let! roll = die
      let result = 
            if roll >= plus then pass roll
            else fail roll
      return result
}
let d6 = uniformDistribution [1..6]
let d3 = uniformDistribution [1..3]
let reduce act = 
      match act with 
      | Value x -> always (pass x)
      | DPlus x -> dPlus x d6
      | Dice (d,count)  -> 
            match d with 
            | D3 -> takeN d3 count >>= (List.sum >> pass >> always)
            | D6 -> takeN d6 count >>= (List.sum >> pass >> always)
      | NoValue -> always (Fail 0.0)
let showProbabilitiesOfActions (key, Ability act) = 
      let probabilities (dist:Distribution<_>) = 
            let result = 
                  dist 
                  |> List.choose(function (Pass x, prob) -> Some(x,prob) | (Fail _,_) -> None)
                  |> List.groupBy fst
                  |> List.map(fun (f,probs) -> f, List.sumBy snd probs)
            let total = result |> List.maxBy snd |> snd
            result
                  |> List.map (fun (f, prob) -> 
                        let proabilityGreen = prob / total * 255.
                        div [Style [Color (proabilityGreen |> int |> sprintf "#77%02X00")]] [str (sprintf "%f" f)])
             |> div [ClassName "column"]            
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column"] [str " => "]
            reduce act |> probabilities           
          ]
let showReducedActions (key, Ability act) = 
      let expectations (dist:Distribution<_>) = 
            let result = dist |> expectation (function Pass _ -> float 0x00FF00  | Fail _ -> float 0xFF0000) |> int
            let colour = sprintf "#%06X" result
            let text = sprintf "%.2f" (dist |> expectation (function Pass x | Fail x -> float x))
            div [ClassName "column"; Style [Color colour]] [str text]
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column"] [str " => "]
            reduce act |> expectations           
          ]
let showSample (key, Ability act) = 
      let sampleDistribution (dist:Distribution<_>) = 
            let result = dist |> sample 
            let colour = (match result with | Pass _ -> float 0x00FF00  | Fail _ -> float 0xFF0000) |> int
            let colour' = sprintf "#%06X" colour
            div [ClassName "column"; Style [Color colour]] [result |> getResult |> string |> str]
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column"] [str " => "]
            reduce act |> sampleDistribution           
          ]
let showActions (key, Ability act) = 
  let showAttr = 
        match act with 
        | DPlus i -> string i + "+"
        | Dice (d,count)  -> 
            match d with 
            | D3 -> string count + "D3"
            | D6 -> string count + "D6"
        | Value i -> string i
        | NoValue -> "--"
  div []
      [ b [] [str key; str " : "]
        str showAttr ]

let showAttributes (key,Characteristic attr) = 
  let showAttr = 
        match attr with 
        | (DPlus i) -> string i + "+"
        | (Dice (d,c)) -> string c + string d
        | (Value i) -> string i
        | (NoValue) -> "--"
  div [ClassName "has-text-centered column"]
      [ b  [] [str key]
        br []
        str showAttr ]
let root model dispatch =
      g []
       [ circle [   Cx !^ model.posX :> IProp
                    Cy !^ model.posY :> IProp
                    R !^ "3" :> IProp
                    OnClick (fun mouseEvt -> Select |> dispatch) :> IProp] []
         text [     TextAnchor "middle"
                    X (!^ model.posX)
                    Y (!^ (model.posY + 7.))
                    StrokeWidth (!^ ".1")
                    Fill "#000000"
                    Stroke "#000000"
                    FontSize !^ "4"    ] 
               [ str model.name]
       ]

