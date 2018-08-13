module MathHammer.Models.View

open Fable.Helpers.React
open Props
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.State
open Probability.View

let onClick x : IProp = OnClick(x) :> _
let showActions dispatch operation =
    GameActions.Primitives.View.probabilities operation dispatch
let showAverages dispatch operation =
    GameActions.Primitives.View.averages operation dispatch
let showSamples dispatch operation =
    GameActions.Primitives.View.sample operation dispatch

let showAttributes ((key : string), operation) dispatch =
    div [ ClassName "has-text-centered column" ] 
        [ b [] [ str key ]
          br []
          GameActions.Primitives.View.probabilities operation dispatch ]

let rangeStops (dist : Distribution.Distribution<_>) =
    let minRange, maxRange, _, _ =
        { dist with Probabilities = dist.Probabilities |> List.rev }
        |> Distribution.choose (function 
               | IntCheck(i) -> Check.map ((*) 1<inch>) i |> Some
               | _ -> None)
        |> Distribution.get
        |> List.fold 
               (fun (currMinRange, currMaxRange, currMin, currMax) (range, prob) -> 
               min currMinRange range, max currMaxRange range, min currMin prob, 
               max currMax prob) 
               (Check.Pass(28<ft> * 12<inch/ft>), Check.Pass 0<inch>, 1., 0.)
        |> function 
        | (Check.Pass minRange, Check.Pass maxRange, minProbability, 
           maxProbability) -> 
            inch.ToMM(int minRange * 1<inch>), inch.ToMM(int maxRange * 1<inch>), 
            minProbability, maxProbability
        | (Check.Fail _, Check.Pass maxRange, minProbability, maxProbability) -> 
            0<mm>, inch.ToMM(int maxRange * 1<inch>), minProbability, 
            maxProbability
        | (_, _, _, _) -> 0<mm>, 0<mm>, 0., 0.
    
    let stopPercent i length = float (i + 1) / float length |> sprintf "%.2f"
    
    let percentGreen (range : int<mm>) =
        if maxRange - minRange = 0<mm> then "#00FF00"
        else 
            colour 
            <| (1. - (float (range - minRange) / float (maxRange - minRange))) 
               * 255.
    
    let stopsPercentGreenAndOpacity (dist : Distribution.Distribution<_>) =
        let length = List.length dist.Probabilities
        
        let stops =
            { dist with Probabilities = dist.Probabilities |> List.rev }
            |> Distribution.get
            |> List.toArray
            |> Array.mapi (fun i (range, prob) -> 
                   match range, prob with
                   | Check(Check.Fail _), _ | _, 0.0 -> 
                       (stopPercent i length, colour 255.), 0.0
                   | Check(Check.Pass(Int(range))), _ -> 
                       (stopPercent i length, 
                        percentGreen (inch.ToMM(range * 1<inch>))), prob
                   | Check(Check.Pass(Float(range))), _ -> 
                       (stopPercent i length, 
                        percentGreen 
                            (int 
                                 (System.Math.Round
                                      (float <| inch.ToMMf(range * 1.<inch>))) 
                             * 1<mm>)), prob
                   | Int(range), _ -> 
                       (stopPercent i length, 
                        percentGreen (inch.ToMM(range * 1<inch>))), prob
                   | Float(range), _ -> 
                       (stopPercent i length, 
                        percentGreen 
                            (int 
                                 (System.Math.Round
                                      (float <| inch.ToMMf(range * 1.<inch>))) 
                             * 1<mm>)), prob
                   | _ -> failwith "invalid range calculation")
            |> Array.rev
        stops
        |> Array.scan 
               (fun ((_, _), lastProb) ((stopPercent, green), prob) -> 
               ((stopPercent, green), prob + lastProb)) 
               (("0.00", "#000000"), 0.0)
        |> List.ofArray
        |> List.skip 1
        |> List.sortByDescending snd
        |> List.map (fun ((offset : string, stopcolor : string), opacity : float) -> 
               stop [ SVGAttr.Offset offset
                      SVGAttr.StopColor stopcolor
                      SVGAttr.StopOpacity opacity ] [])
    
    (minRange, maxRange, stopsPercentGreenAndOpacity dist)

let safe (id : string) =
    match id with
    | null -> ""
    | id -> id.Replace(" ", "")

let groupFor model display =
    g [ Id model.Name
        SVGAttr.Transform(sprintf "translate(%f,%f)" model.PosX model.PosY) ] 
        [ g [] display ]

let rangeRoot name model =
    let dist =
        model.ProbabilityRules 
        |> Option.map (getp name >> evalOp Map.empty)
    
    let ranges id (_ : int<mm>, max : int<mm>, stops) =
        g [] [ defs [] [ radialGradient [ Id(safe id) ] stops ]
               circle [ SVGAttr.Fill(sprintf "url(#%s)" (safe id))
                        R(float max) ] [] ]
    dist
    |> Option.bind (function 
           | IsDistribution d -> Some d
           | Value(Int(i)) -> 
               Some(Check.Pass(Int(i))
                    |> Check
                    |> Distribution.always)
           | _ -> None)
    |> Option.map (rangeStops
                   >> ranges name
                   >> List.singleton
                   >> groupFor model)

let root model dispatch =
    let modelDisplay =
        [ circle [ R(model.Size / 2 |> float)
                   OnMouseOver(fun _ -> Select |> dispatch)
                   Class "draggable" ] []
          text [ SVGAttr.TextAnchor "middle"
                 SVGAttr.Y 50.
                 SVGAttr.StrokeWidth(".5")
                 SVGAttr.Fill "#000000"
                 SVGAttr.Stroke "#000000"
                 SVGAttr.FontSize "25" ] [ str model.Name ] ]
    groupFor model modelDisplay
