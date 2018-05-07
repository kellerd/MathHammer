module MathHammer.UnitList.View

open Fable.Helpers.React
open Props
open Types

let rootBoard model _ = 
    g   [SVGAttr.Transform <| sprintf "translate(%d,%d)" model.Location.Dimensions.Left model.Location.Dimensions.Top ]
        [g  [ ] 
            [rect 
                [ SVGAttr.Width model.Location.Dimensions.Width
                  SVGAttr.Height model.Location.Dimensions.Height
                  SVGAttr.Fill model.Location.Fill] []
             g [SVGAttr.Transform <| sprintf "translate(%d,%d)" model.Deployment.Left model.Deployment.Top] 
               [ svg 
                    [ SVGAttr.Width model.Deployment.Width
                      SVGAttr.Height model.Deployment.Height ]
                    [ rect [ SVGAttr.Width model.Deployment.Width
                             SVGAttr.Height model.Deployment.Height
                             SVGAttr.Fill model.DeploymentFill] []
                      text [ SVGAttr.TextAnchor "middle"
                             SVGAttr.StrokeWidth ".5"
                             SVGAttr.Fill "#999999"
                             SVGAttr.Stroke "#999999"
                             SVGAttr.FontSize "25" 
                             X "50%"
                             Y "50%" ] [str "Deployment Area"] ] ]          
            ]
        ]
let rootRanges model key name = 
    Map.tryFind name model.Models
    |> Option.bind (MathHammer.Models.View.rangeRoot key)
    |> Option.map (fun distance -> 
        g   [SVGAttr.Transform <| sprintf "translate(%d,%d)"  model.Location.Dimensions.Left (model.Location.Dimensions.Top + model.Deployment.Top) ]
            [ g  [ SVGAttr.Stroke model.ElementStroke; SVGAttr.StrokeWidth "1" ] [ distance ] ])

let root model dispatch =
    let models = 
        model.Models 
        |> Map.toList
        |> List.map (fun (_,m) -> MathHammer.Models.View.root m (fun msg -> ModelMsg(msg,m.Name) |> dispatch))
        |> g [ SVGAttr.Fill model.ElementFill ; SVGAttr.Stroke model.ElementStroke; SVGAttr.StrokeWidth "1" ]
    g   [ SVGAttr.Transform <| sprintf "translate(%d,%d)"  model.Location.Dimensions.Left (model.Location.Dimensions.Top + model.Deployment.Top) ]
        [ models ]
      
