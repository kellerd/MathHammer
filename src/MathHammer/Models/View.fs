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
      | Value v -> reduceGamePrimitive v |> map pass
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
      | Multiply (op,op2) -> 
            dist {
                  let! result1 = reduce op
                  let! result2 = reduce op2
                  return result1 * result2
            }
      | DPlus(d, moreThan) -> reduceDie d |> dPlus moreThan
      | Count ops -> dist {
            let! ds = traverseResultM reduce ops 
            let counts =
                 ds |> List.countBy(function | Pass _ -> pass 1 | Fail _ -> fail 1 | _ -> failwith "Cannot count these") 
                    |> List.map(function (Pass _,count) -> pass count | (Fail _, count) -> fail count | _ -> failwith "Cannot count these")
            return List counts              
            }
      | Var(_, _) -> failwith "Not Implemented"
      | Let(_, _, _) -> failwith "Not Implemented" 
 
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
            div [ClassName "column  is-narrow"] [str " => "]
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
      | Multiply (Value(Int(x)),Value(Int(y))) -> sprintf "%d" (x * y)
      | Multiply (Value(Dice(d)),Value(Int(i))) 
      | Multiply (Value(Int(i)),Value(Dice(d))) -> sprintf "%d * %s" i (printDs d)
      | Multiply (v,i) -> sprintf "(%s * %s)" (showAttr v) (showAttr i)
      | DPlus (D6,i) -> string i + "+"
      | DPlus (D3,i) -> string i + "+ on D3"
      | Total (ops) when List.distinct ops = [Value(Dice(D6))] -> sprintf "Total(%dD6)" (List.length ops)
      | Total (ops) when List.distinct ops = [Value(Dice(D3))] -> sprintf "Total(%dD3)" (List.length ops)
      | Total (ops)  -> sprintf "Total(%s)" (List.map showAttr ops |> String.concat " + ")
      | Value i -> string i
      | Value i -> string i
      | NoValue -> "--"
      | DPlus(Reroll(is,D6), i) -> sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is))
      | DPlus(Reroll(is,D3), i) -> sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is))
      | DPlus(Reroll(is,Reroll(is2,d)), i) -> showAttr (DPlus(Reroll(List.distinct (is @ is2),d),i))
      | Count(ops) -> sprintf "(Passes,Fails) in (%s)" (List.map showAttr ops |> String.concat ",")
      | Var(env, str) -> match env with 
                           | Attacker -> sprintf "Attacker(%s)" str
                           | Defender -> sprintf "Target(%s)" str
                           | Global -> sprintf "Global(%s)" str
      | Let(env, str, op) ->  
            match env with 
            | Attacker -> sprintf "let Attacker(%s) = %s" str  (showAttr op)
            | Defender -> sprintf "let Target(%s) = %s" str (showAttr op)
            | Global -> sprintf "let Global(%s) = %s" str (showAttr op)
  div []
      [ b [] [str key; str " : "]
        showAttr act |> str  ]

let showAttributes (key,Characteristic attr) dispatch = 
  
  div [ClassName "has-text-centered column"]
      [ b  [] [str key]
        br []
        GameActions.Primitives.View.root attr dispatch ]
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

