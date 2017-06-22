module MathHammer.Models.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Fable.Import
open Probability
open MathHammer.GameActions.Types
open Result
let onClick x : IProp = OnClick(x) :> _


let pass i = float i |> Pass
let fail i = float i  |> Fail
let list d = List d 
let tuple d = Tuple d
let getResult = function Pass f | Fail f -> Some f | _ -> None
let dPlus plus die = dist {
      let! roll = die
      let result = 
            if roll >= plus then pass roll
            else fail roll
      return result
}
let d6 = uniformDistribution [1..6]
let d3 = uniformDistribution [1..3]
let rec reduceDie d : Distribution<_> = 
      match d with 
      | D3 -> d3
      | D6 -> d6
      | Reroll(rerolls, d) -> 
            dist {
                  let! roll = reduceDie d
                  if List.contains roll rerolls then
                        return! reduceDie d
                  else return roll                        
            }
let reduceGamePrimitive = function
      | Int i -> always i
      | Dice d -> reduceDie d 
let rec reduce operation = 
      match operation with
      | Value v -> reduceGamePrimitive v |> Probability.map pass
      | NoValue -> fail 0 |> always
      | Total (ops) -> 
            ops 
            |> List.map reduce
            |> List.reduce (fun a b -> 
                  dist {
                        let! a' = a
                        let! b' = b
                        return a' + b'                              
                  }
            )
      | Many (op,count) -> 
            dist {
                  let! results = takeN (reduce op) count   
                  return list results
            }
      | DPlus(d, moreThan) -> reduceDie d |> dPlus moreThan 
let showProbabilitiesOfActions (key, Ability act) = 
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
            div [ClassName "column"] [str " => "]
            reduce act |> probabilities           
          ]
let showAverages (key, Ability act) = 
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
            reduce act |> expectations           
          ]
let showSample (key, Ability act) = 
      let sampleDistribution (dist:Distribution<_>) = 
            let result = dist |> sample 
            let colour = (match result with Pass _ -> float 0x00FF00  | Fail _ -> float 0xFF0000 | Tuple _ | List _-> float 0x000000) |> int
            let colour' = sprintf "#%06X" colour
            div [ClassName "column"; Style[Color colour']] [printResult result |> str ]
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column"] [str " => "]
            reduce act |> sampleDistribution           
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

let showActions (key, Ability act) = 
  let rec showAttr act = 
      match act with 
      | Many (Value(Dice(d)),i) -> sprintf "%d%s" i (printDs d)
      | Many (v,i) -> sprintf "(%s * %d)" (showAttr v) i
      | DPlus (d,i) -> string i + "+"
      | Total (ops) when List.distinct ops = [Value(Dice(D6))] -> sprintf "Total(%dD6)" (List.length ops)
      | Total (ops) when List.distinct ops = [Value(Dice(D3))] -> sprintf "Total(%dD3)" (List.length ops)
      | Total (ops)  -> sprintf "Total(%s)" (List.map showAttr ops |> String.concat " + ")
      | Value i -> string i
      | NoValue -> "--"
  div []
      [ b [] [str key; str " : "]
        showAttr act |> str  ]

let showAttributes (key,Characteristic attr) = 
  let rec showAttr act = 
      match attr with 
      | Many (op,count) -> sprintf "%d %s" count (showAttr op)
      | DPlus (_,i) -> string i + "+"
      | Total (ops) when List.distinct ops = [Value(Dice(D6))] -> sprintf "Total(%dD6)" (List.length ops)
      | Total (ops) when List.distinct ops = [Value(Dice(D3))] -> sprintf "Total(%dD3)" (List.length ops)
      | Total (ops)  -> sprintf "Total(%s)" (List.map showAttr ops |> String.concat " + ")
      | Value i -> string i
      | NoValue -> "--"
  div [ClassName "has-text-centered column"]
      [ b  [] [str key]
        br []
        showAttr attr |> str  ]
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

