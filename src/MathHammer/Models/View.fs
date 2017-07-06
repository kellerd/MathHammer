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
let list d = List d 
let tuple d = Tuple d
let getResult = function Pass f | Fail f -> Some f | _ -> None



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
        dist 
        |> List.mapi (fun i (range,prob) -> 
            match range,prob with 
            | Fail _,_ | _,0.0 -> stopPercent i length, "#FF0000", "0.0"
            | Pass range,_ -> stopPercent i length, percentGreen (inch.ToMM(int range * 1<inch>)), opacity prob
            | _ -> failwith "invalid range calculation")  
    (minRange,maxRange, 
     stopsPercentGreenAndOpacity  
     |> List.map(fun (offset,stopcolor,opacity) -> 
                        stop [ Offset !^ offset
                               StopColor stopcolor
                               StopOpacity !^ opacity ] []))

let groupFor model display = 
      g     [Transform <| sprintf "translate(%f,%f)" model.PosX model.PosY]
            [ g   [ Transform model.Scale ] display ]

let rangeRoot model name dist =
      let ranges id (min:int<mm>,max:int<mm>,stops) = 
            g [] 
              [   defs  [] 
                        [ radialGradient [ Id id ]
                                           stops ]
                  circle [Fill <| sprintf "url(#%s)" id
                          R !^ (float max)] [] ]
      [ ranges name <| rangeStops dist]  
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

