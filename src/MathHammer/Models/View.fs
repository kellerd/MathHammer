module MathHammer.Models.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Fable.Import

let onClick x : IProp = OnClick(x) :> _

let d6 = evenDistribution [1..6] 
let d3 = evenDistribution [1..3] 
type Result = Pass of int | Fail of int
let dPlus plus die = dist {
      let! roll = die
      let result = 
            if roll >= plus then Pass roll
            else Fail roll
      return result
}

let reduce act = 
      match act with 
      | Value x -> always (Pass x)
      | DPlus x -> dPlus x d6
      | Dice ds -> failwith "x"
      | NoValue -> always (Fail 0)


let showActions (key, Ability act) = 
  let showAttr = 
        match act with 
        | (DPlus i) -> string i + "+"
        | (Dice ds) -> 
            let (d3s,d6s) = ds |> List.partition (function D3 -> true | D6 -> false)
            let (d3len,d6len) = (List.length d3s, List.length d6s)
            (if d3len > 0 then string d3len + "D3" else "") + 
              (if d6len > 0 then string d6len + "D6" else "" )
        | (Value i) -> string i
        | (NoValue) -> "--"
        | _ -> "FUCKCKX"
  div []
      [   div []
              [ b  [] [str key; str " : "]
                str showAttr ]
          div []
              [ str " => "
                br []
                reduce act ]
      ]

let showAttributes (key,Characteristic attr) = 
  let showAttr = 
        match attr with 
        | (DPlus i) -> string i + "+"
        | (Dice i) -> "D" + string i 
        | (Value i) -> string i
        | (NoValue) -> "--"
        | _ -> "FUCKCK"
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

