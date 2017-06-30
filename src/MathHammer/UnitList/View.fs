module MathHammer.UnitList.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open GameActions.Primitives.Types



let root model dispatch =
    let models = 
        model.Models 
        |> Map.toList
        |> List.map (fun (_,m) -> MathHammer.Models.View.root m (fun msg -> ModelMsg(msg,m.name) |> dispatch))
        |> g [Fill model.ElementFill ; Stroke model.ElementStroke; StrokeWidth (!^ "1")]
    let deploymentOffset = 
        if model.OffsetY = 0<mm> then 0<mm>
        else model.OffsetY + model.Deployment

    g []
        (rect 
            [ SVGAttr.Y !^ (float model.OffsetY)
              unbox ("width", (model.Width |> string))
              unbox ("height", (model.Height |> string))
              Fill model.BoxFill] []
        :: rect 
            [ SVGAttr.Y (!^ (float deploymentOffset))
              unbox ("width", (model.Width |> string))
              unbox ("height", (model.Deployment |> string))
              Fill model.DeploymentFill] []
        :: [models])
      
