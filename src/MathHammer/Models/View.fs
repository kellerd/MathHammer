module MathHammer.Models.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types

[<Emit("$0")>] 
let svgRaw (text:string) : Fable.Import.React.ReactElement = jsNative
let root model dispatch =
      g []
       [ circle [   Cx !^ model.posX
                    Cy !^ model.posY
                    R !^ "3"            ] []
         text [     TextAnchor "middle"
                    X (!^ model.posX)
                    Y (!^ (model.posY + 7.))
                    StrokeWidth (!^ ".1")
                    Fill "#000000"
                    Stroke "#000000"
                    FontSize !^ "4"     ] 
              [svgRaw model.name] 
       ]

