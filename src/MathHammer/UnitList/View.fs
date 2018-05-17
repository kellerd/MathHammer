module MathHammer.UnitList.View

open Fable.Helpers.React
open Props
open Types

let rootLocation model =
    let optionalLabel =
        model.Label
        |> Option.map (fun label -> 
               text [ SVGAttr.TextAnchor "middle"
                      SVGAttr.StrokeWidth ".5"
                      SVGAttr.Fill "#999999"
                      SVGAttr.Stroke "#999999"
                      SVGAttr.FontSize "25"
                      X "50%"
                      Y "50%" ] [ str label ])
        |> ofOption
    g [ SVGAttr.Transform <| sprintf "translate(%d,%d)" model.Dimensions.Left model.Dimensions.Top ] 
        [ svg [ SVGAttr.Width model.Dimensions.Width
                SVGAttr.Height model.Dimensions.Height ] [ rect [ SVGAttr.Width model.Dimensions.Width
                                                                  SVGAttr.Height model.Dimensions.Height
                                                                  SVGAttr.Fill model.Fill ] []
                                                           optionalLabel ] ]

let rootRanges model key name =
    Map.tryFind name model.Models
    |> Option.bind (MathHammer.Models.View.rangeRoot key)
    |> Option.map (fun distance -> 
           g [] [ g [ SVGAttr.Stroke model.ElementStroke
                      SVGAttr.StrokeWidth "1" ] [ distance ] ])

let root model dispatch =
    let models =
        model.Models
        |> Map.toList
        |> List.map (fun (_, m) -> MathHammer.Models.View.root m (fun msg -> ModelMsg(msg, m.Name) |> dispatch))
        |> g [ SVGAttr.Fill model.ElementFill
               SVGAttr.Stroke model.ElementStroke
               SVGAttr.StrokeWidth "1" ]
    g [] [ models ]
