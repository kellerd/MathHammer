module MathHammer.Models.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Fable.Import

let onClick x : IProp = OnClick(x) :> _
let showAttributes (key,attr) = 
  let showAttr = 
        match attr with 
        | DPlus i -> string i + "+"
        | Dice i -> "D" + string i 
        | Value i -> string i
  div [ClassName "has-text-centered column"]
      [ b  [] [str key]
        br []
        str showAttr ]
let root model dispatch =
      g []
       [ circle [   Cx !^ model.posX :> IProp
                    Cy !^ model.posY :> IProp
                    R !^ "3" :> IProp
                    OnClick (fun mouseEvt -> Select |> dispatch) :> IProp] []
         text [     TextAnchor "middle"
                    X (!^ model.posX)
                    Y (!^ (model.posY + 7.))
                    StrokeWidth (!^ ".1")
                    Fill "#000000"
                    Stroke "#000000"
                    FontSize !^ "4"    ] 
               [ str model.name]
       ]

