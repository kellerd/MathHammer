module MathHammer.UnitList.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types




let root model dispatch =
  let models = 
    model.Models 
    |> List.map (fun m -> MathHammer.Model.View.root m dispatch)
    |> g [Fill model.ElementFill ; Stroke model.ElementStroke; StrokeWidth (!^ "1")]
  g []
    (rect 
         [ SVGAttr.Y (!^ model.OffsetY)
           unbox ("width", "100%")
           unbox ("height", "50%")
           Fill model.BoxFill] []
      :: [models])
      
