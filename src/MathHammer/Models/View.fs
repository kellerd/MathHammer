module MathHammer.Models.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Fable.Import
open Probability

let onClick x : IProp = OnClick(x) :> _


type Result = Pass of float | Fail of float | List of Result list | Tuple of int * int
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
      | Sum (a,b) -> dist {
            let! a' = reduceGamePrimitive a
            let! b' = reduceGamePrimitive b
            return pass(a' + b')
       }
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
                  |> List.choose(function (Pass x, prob) -> Some(x,prob) | (Fail _,_) | (Tuple _,_) | (List _,_)-> None)
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
            let result = dist |> expectation (function Pass _ -> float 0x00FF00  | Fail _ -> float 0xFF0000 | Tuple _ | List _-> float 0x000000) |> int
            let colour = sprintf "#%06X" result
            let text = sprintf "%.2f" (dist |> List.choose (fun (result',prob) -> match result' with 
                                                                                  | Pass x | Fail x  -> Some((result',prob))
                                                                                  | _ -> None) |> expectation (function Pass x | Fail x -> float x | _ -> -1.))
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
            let colour = (match result with Pass _ -> float 0x00FF00  | Fail _ -> float 0xFF0000 | Tuple _ | List _-> float 0x000000) |> int
            let colour' = sprintf "#%06X" colour
            div [ClassName "column"; Style [Color colour]] [result |> getResult |> string |> str]
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
      let str = "Dabd6Dd6D3D6D6D3aD3"
      let reduceD6 str  =
            (str:string).ToCharArray() 
            |> Seq.fold(fun (count,d6,(text:string)) nextChar -> 
                          match d6,nextChar with 
                          | [||], 'D'-> (1,[|'D'|],text)
                          | [||], c-> 0,[||],sprintf "%s%c" text c
                          | [|'D'|], ('6' as n) | [|'D'|], ('3' as n)-> count,[| 'D'; n |],text
                          | [|'D'|], _ -> 0,[||], sprintf "%s%c%c" text 'D' nextChar
                          | [|'D';'6'|], 'D'-> (count,[|'D';'6';'D'|],text)
                          | [|'D';'3'|], 'D'-> (count,[|'D';'3';'D'|],text)
                          | [|'D';'6';'D'|], '6'-> (count+1,[|'D';'6'|],text)
                          | [|'D';'6';'D'|], '3'-> 
                              if count = 1 then  1,[|'D';'3'|],text + "D6"
                              else 1,[|'D';'3'|],text + count.ToString() +  "D6"
                          | [|'D';'3';'D'|], '3'-> (count+1,[|'D';'3'|],text)
                          | [|'D';'3';'D'|], '6'-> 
                              if count = 1 then  (1,[|'D';'6'|],text + "D3")
                              else (1,[|'D';'6'|],text + count.ToString() +  "D3") 
                          | cs ,_ -> 
                              if count = 1 then  0,[||], text + (System.String(cs)) + nextChar.ToString()
                              else 0,[||],text + count.ToString() + (System.String(cs)) + nextChar.ToString()
                          | _ ->  0,[||],text + nextChar.ToString()
                          ) (0,[||],"")
            |> (function 
                  | count, [|d;n;x|], text ->
                        if count = 1 then  sprintf "%s%c%c%c" text d n x
                        else sprintf "%s%d%c%c%c" text count d n x
                  | count, [|d;n;|], text ->
                        if count = 1 then  sprintf "%s%c%c" text d n
                        else sprintf "%s%d%c%c" text count d n 
                  | _, d, text -> sprintf "%s%s" text (System.String(d)))
      reduceD6 str
      match act with 
      | Many (op,count) -> sprintf "%d%s" count (showAttr op)
      | DPlus (d,i) -> string i + "+"
      | Sum (a, b)  -> printPrimitive a + printPrimitive b |> reduceD6
      | Value i -> string i
      | NoValue -> "--"
  div []
      [ b [] [str key; str " : "]
        showAttr act |> str  ]

let showAttributes (key,Characteristic attr) = 
  let rec showAttr act = 
        match attr with 
        | Many (op,count) -> sprintf "%d%s" count (showAttr op)
        | DPlus (_,i) -> string i + "+"
        | Sum (d,c) -> string c + string d
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

