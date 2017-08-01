module MathHammer.Models.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Fable.Import
open GameActions.Primitives.Types
open Result
open Distribution
open Probability.View
let onClick x : IProp = OnClick(x) :> _

let showActions dispatch (key, operation)  = 
      div [] 
          [ b [] [str key; str " : "]
            GameActions.Primitives.View.root operation dispatch ]

let showAttributes (key, operation) dispatch = 
      div [ClassName "has-text-centered column"]
          [ b  [] [str key]
            br []
            GameActions.Primitives.View.root operation dispatch ]
let rangeStops (dist:Distribution<_>)  = 
    let length = List.length dist
    let minRange, maxRange,minProbability,maxProbability =
        dist 
        |> Distribution.choose (function IntResult (i) -> Result.map ((*) 1<inch>) i |> Some | _ -> None)
        |> List.fold (fun (currMinRange,currMaxRange,currMin,currMax) (range,prob) -> 
            min currMinRange range, max currMaxRange range,
            min currMin prob, max currMax prob ) (Pass (28<ft> * 12<inch/ft>),Pass 0<inch>,1.,0.)
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
        else colour <| (1. - (float (range - minRange) / float(maxRange - minRange))) * 255. 

    let stopsPercentGreenAndOpacity = 
        let stops = 
            dist 
            |> List.toArray
            |> Array.mapi (fun i (range,prob) -> 
                match range,prob with 
                | Result(Fail _),_ | _,0.0 -> (stopPercent i length, colour 255.), 0.0
                | Result(Pass (Int(range))),_ -> (stopPercent i length, percentGreen (inch.ToMM(int range * 1<inch>))), prob
                | _ -> failwith "invalid range calculation") 
            |> Array.rev
        stops
        |> Array.scan (fun ((_,_),lastProb) ((stopPercent,green),prob) -> ((stopPercent,green),prob+lastProb)) (("0.00","#000000"), 0.0)                          
        |> List.ofArray 
        |> List.skip 1
        |> List.sortByDescending snd
        |> List.map(fun ((offset:string,stopcolor:string),opacity:float) -> 
                        stop [ Offset !^ offset
                               StopColor stopcolor
                               StopOpacity !^ opacity ] [])
        
    (minRange,maxRange, stopsPercentGreenAndOpacity)
     
let showProbabilitiesOfActions (key,dist) = 
      let probabilities (dist:Distribution<GamePrimitive>) = 
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
                              match r with 
                              | IntResult r -> 
                                  let greenValue = Result.map float r |> passFailToExpectation
                                  //let percentageGreen = prob / max * 255.
                                  let alpha = opacity min max prob
                                  //let colour = sprintf "#%02X%02X00" (0xFF - System.Convert.ToInt32 percentageGreen) (System.Convert.ToInt32(percentageGreen))
                                  div [Style [Color (colourA greenValue alpha)]] [str (printResultD r); str <| sprintf " %.2f%%" (prob / total)]
                              | v -> div [] [GameActions.Primitives.View.unparseValue v |> str; str <| sprintf " %.2f%%" (prob / total)])
                    |> div [ClassName "column"]            
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            dist |> probabilities           
          ]

let showSample (key, dist) = 
      let sampleDistribution dist = 
            match sample dist with 
            | IntResult result -> 
                let colour' = result  |> Result.map float |> passFailToExpectation  |> colour
                div [ClassName "column"; Style[Color colour']] [printResultD result |> str ]
            | v -> div [ClassName "column";] [GameActions.Primitives.View.unparseValue v |> str]
      section [ClassName "columns"]
          [ 
            div [ClassName "column"] [b  [] [str key]]
            dist |> sampleDistribution           
          ]        

let groupFor model display = 
      g     [Transform <| sprintf "translate(%f,%f)" model.PosX model.PosY]
            [ g   [ Transform model.Scale ] display ]
let rangeRoot name model =
    let dist = Map.tryFind name model.Environment
    let ranges id (min:int<mm>,max:int<mm>,stops) = 
        g [] 
          [   defs  [] 
                    [ radialGradient [ Id id ]
                                       stops ]
              circle [Fill <| sprintf "url(#%s)" id
                      R !^ (float max)] [] ]

    dist
    |> Option.bind(|IsDistribution|_|)
    |> Option.map 
        (rangeStops 
        >> ranges name
        >> List.singleton
        >> groupFor model )
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

