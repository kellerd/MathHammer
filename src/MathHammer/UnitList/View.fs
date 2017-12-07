module MathHammer.UnitList.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open GameActions.Primitives.Types


let rootBoard model dispatch = 
    g   [SVGAttr.Transform <| sprintf "translate(0,%d)" (model.OffsetY * 2) ]
        [g  [SVGAttr.Transform model.Scale ] 
            [rect 
                [ unbox ("width", (model.Width |> string))
                  unbox ("height", (model.Height |> string))
                  SVGAttr.Fill model.BoxFill] []
             rect 
                [ 
                  unbox ("width", (model.Width |> string))
                  unbox ("height", (model.Deployment |> string))
                  SVGAttr.Fill model.DeploymentFill] []
             g   [SVGAttr.Transform <| sprintf "translate(%d,%d)" (model.Width / 2) (model.Deployment / 2) ]
                [   g   [SVGAttr.Transform model.Scale ] 
                        [text [ TextAnchor "middle"
                                SVGAttr.StrokeWidth ".5"
                                SVGAttr.Fill "#999999"
                                Stroke "#999999"
                                SVGAttr.FontSize "25"    ] [str "Deployment Area"] ] 
                ]
            ]
        ]
let rootRanges model key name = 
    Map.tryFind name model.Models
    |> Option.bind (MathHammer.Models.View.rangeRoot key)
    |> Option.map (fun distance -> 
        g   [SVGAttr.Transform <| sprintf "translate(0,%d)" (model.OffsetY * 2) ]
            [ g  [SVGAttr.Transform model.Scale ] [ g  [ Stroke model.ElementStroke; SVGAttr.StrokeWidth "1" ] [ distance ] ] ])

let root model dispatch =
    let models = 
        model.Models 
        |> Map.toList
        |> List.map (fun (_,m) -> MathHammer.Models.View.root m (fun msg -> ModelMsg(msg,m.Name) |> dispatch))
        |> g [ SVGAttr.Fill model.ElementFill ; Stroke model.ElementStroke; SVGAttr.StrokeWidth "1" ]
    g   [ SVGAttr.Transform <| sprintf "translate(0,%d)" (model.OffsetY * 2) ]
        [ g  [SVGAttr.Transform model.Scale ] [ models ] ]
      
