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
let rec reduce env operation = 
      let (attackers,defenders,globals) = env
      match operation with
      | Value v -> env,reduceGamePrimitive v |> map pass
      | NoValue -> env,always (fail 0)
      | Total ([]) -> env,always (pass 0)
      | Total (op::rest) -> 
            let state = reduce env op
            rest 
            |> List.fold (fun (env1,reduced1) op -> 
                  let (env2,reduced2) = reduce env op
                  env2, dist {
                        let! a' = reduced1
                        let! b' = reduced2
                        return a' + b'                              
                  }) state
      | Multiply (op,op2) -> 
            let (env1,reduced1) = reduce env op
            let (env2,reduced2) = reduce env1 op2
            env2,
            dist {

                  let! result1 = reduced1
                  let! result2 = reduced2
                  return result1 * result2
            }
      | DPlus(d, moreThan) -> env,reduceDie d |> dPlus moreThan
      | Count ([]) -> env,always (pass 0)
      | Count (op::rest) -> 
            let toCount (env1,result) =
                  env1, dist {
                        let! result = result 
                        return match result with | Pass _ -> List [pass 1; fail 0] | Fail _ -> List [pass 0; fail 1] | _ -> failwith "Cannot count these" 
                  }
            let state = reduce env op |> toCount
            rest 
            |> List.fold (fun (env1,reduced1) op -> 
                  let (env2,reduced2) = reduce env1 op |> toCount
                  env2,dist {
                        let! count1 = reduced1
                        let! count2 = reduced2 
                        return count1 + count2
                  }                  
            ) state
      | Var(Attacker, var) -> env,(Map.tryFind var attackers |> Option.defaultValue (reduce env NoValue |> snd) )
      | Var(Defender, var) -> env,Map.tryFind var defenders |> Option.defaultValue (reduce env NoValue |> snd)
      | Var(Global, var) -> env,Map.tryFind var globals |> Option.defaultValue (reduce env NoValue |> snd )
      | Let(Attacker, var, op) -> 
            let ((attackers',defenders',globals'),result) = reduce env op
            (Map.add var result attackers, defenders',globals'),result
      | Let(Defender, var, op) -> 
            let ((attackers',defenders',globals'),result) = reduce env op
            (Map.add var result attackers, defenders',globals'),result
      | Let(Global, var, op) -> 
            let ((attackers',defenders',globals'),result) = reduce env op
            (Map.add var result attackers, defenders',globals'),result

let showProbabilitiesOfActions env (key, Ability act) = 
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
            reduce env act |> snd |> probabilities           
          ]
let showAverages env (key, Ability act) = 
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
            reduce env act |> snd |> expectations           
          ]
let showSample env (key, Ability act) = 
      let sampleDistribution (dist:Distribution<_>) = 
            let result = dist |> sample 
            let colour = (match result with Pass _ -> float 0x00FF00  | Fail _ -> float 0xFF0000 | Tuple _ | List _-> float 0x000000) |> int
            let colour' = sprintf "#%06X" colour
            div [ClassName "column"; Style[Color colour']] [printResult result |> str ]
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            div [ClassName "column"] [str " => "]
            reduce env act |> snd |> sampleDistribution           
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
            let (newEnv,newElement) = ranges name (f env)
            (newEnv,newElement::acc)) (env,[])  
      |> groupFor model 
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

