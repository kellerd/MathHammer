module MathHammer.View

open Fable.Core
open Fable.Core.JsInterop
module R = Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish.React
open Types

let root model dispatch =
  let drawing =   
    R.div [] 
          [R.svg 
            [ ViewBox "0 0 100 100"; unbox ("width", "100%") ]
            [ UnitList.View.root model.Attacker (State.attackerMap >> dispatch)
              UnitList.View.root model.Defender (State.defenderMap >> dispatch) ] ] 
  let swap =  R.i [ClassName "column fa fa-exchange"; OnClick (fun _ -> Swap |> dispatch) ] []
  let selected = 
        let titleBar text = 
           (R.section [ ClassName "hero is-primary  has-text-centered"]
                       [ R.div [ClassName "hero-body"]
                               [ R.h1 [ClassName "title"]
                                      [ R.str text] ] ])
        let sequence = model.Selected |> Option.map (fun selected -> R.div [] [ R.str selected.name ])
        match model.Selected with 
        | None ->          titleBar "<< Select model to edit turn sequence >>"
        | Some selected -> titleBar selected.name
        
        
  R.div [] 
        [ swap
          drawing
          selected  ] 
