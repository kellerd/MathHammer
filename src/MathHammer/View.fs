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
        //let sequence = model.Selected |> Option.map (fun selected -> R.div [] [ R.str selected.name ])
        match model.Selected with 
        | None ->          titleBar "<< Select model to edit turn sequence >>"
        | Some selected -> 
             let title = titleBar selected.name
             let (attrs,actions) = 
                selected.attributes 
                |> List.partition (snd >> MathHammer.Models.Types.isCharacteristic)
             let attrDiv = 
                attrs                 
                |> List.map MathHammer.Models.View.showAttributes
                |> R.div [ClassName "columns"] 
             let (left,right) = 
                actions
                |> List.mapi (fun i m -> i,MathHammer.Models.View.showActions m)
                |> List.partition (fun (i,_) -> i % 2 = 0)
             let actionsDiv = 
                [ left  |> List.map snd |> R.div [ClassName "column"] 
                  right |> List.map snd |> R.div [ClassName "column"]]
                |> R.div [ClassName "columns"] 
             R.section [Id "selected"] (title :: attrDiv :: [actionsDiv])

        
  R.div [] 
        [ swap
          drawing
          selected  ] 
