module MathHammer.View

open Fable.Core
open Fable.Core.JsInterop
module R = Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish.React
open MathHammer.Models.Types
open Types
open GameActions.Primitives.Types
  
let isCharacteristic = function Characteristic x -> true | Ability x -> false

let root model dispatch =
    let (boardX,boardY) = model.Board |> fun (x,y) -> ft.ToMM(x),ft.ToMM(y)

    let drawing =   
        R.div [] 
            [R.svg 
                [ ViewBox (sprintf "0 0 %d %d" boardX boardY); unbox ("width", "100%")]
                [ UnitList.View.root model.Attacker (State.attackerMap >> dispatch)
                  UnitList.View.root model.Defender (State.defenderMap >> dispatch) ] ] 
    let swap =  R.i [ClassName "column fa fa-arrows-v"; OnClick (fun _ -> Swap |> dispatch) ] []
    let selected = 
        let titleBar text = 
            (R.section [ ClassName "hero is-primary  has-text-centered"]
                       [ R.div [ClassName "hero-body"]
                               [ R.h1 [ClassName "title"]
                                      [ R.str text] ] ])
        let bar text = 
            (R.section [ ClassName "hero has-text-centered"]
                       [ R.div [ClassName "hero-body"]
                               [ R.h1 [ClassName "title"]
                                      [ R.str text ] ] ])
        let columnsOf f items =                                       
            let toColumns (left,right) = 
                [left;right]
                |> List.map (List.map snd >> R.div [ClassName "column"])
                |> R.div [ClassName "columns"] 
            items
                 |> List.mapi (fun i m -> i,f m)
                 |> List.partition (fun (i,_) -> i % 2 = 0)
                 |> toColumns                
        //let sequence = model.Selected |> Option.map (fun selected -> R.div [] [ R.str selected.name ])
        match model.Selected with 
        | None ->  titleBar "<< Select model to edit turn sequence >>"
        | Some selected -> 
             let title = titleBar selected.Name
             let (attrs,actions) = 
                 selected.Attributes 
                 |> List.partition (snd >> isCharacteristic)
             let attrDiv = 
                 attrs                 
                 |> List.map (fun attr -> MathHammer.Models.View.showAttributes attr dispatch)
                 |> R.div [ClassName "columns"]  
             let actionsDiv = columnsOf MathHammer.Models.View.showActions actions
             let averagesDiv = columnsOf MathHammer.Models.View.showAverages actions
             let probabiltiesActionsDiv = columnsOf MathHammer.Models.View.showProbabilitiesOfActions actions
             let sampleActionsDiv = columnsOf MathHammer.Models.View.showSample actions

             R.section [Id "selected"] [ title; attrDiv
                                         bar "Actions"; actionsDiv
                                         bar "Averages"; averagesDiv
                                         bar "Probabilities"; probabiltiesActionsDiv
                                         bar "Sample"; sampleActionsDiv ]
    R.div [] [ swap; drawing; selected  ] 