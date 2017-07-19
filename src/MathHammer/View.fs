module MathHammer.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish.React
open MathHammer.Models.Types
open Types
open GameActions.Primitives.Types
  
let isCharacteristic = function 
                        | Value(NoValue)            | Value(_)             | Call(DPlus(_) )
                        | Let(_, _, Value(_)) | Let(_, _, Call(DPlus(_))) | Let(_, _, Call(DPlus(_))) -> true
                        | _ -> false

let root model dispatch =
    let (boardX,boardY) = model.Board |> fun (x,y) -> ft.ToMM(x),ft.ToMM(y)
    let drawing =   
        div [] 
            [svg 
                [ ViewBox (sprintf "0 0 %d %d" boardX boardY); unbox ("width", "100%")]
                [ UnitList.View.rootBoard model.Attacker (State.attackerMap >> dispatch)
                  UnitList.View.rootBoard model.Defender (State.defenderMap >> dispatch)
                  model.SelectedAttacker |> Option.bind(UnitList.View.rootRanges model.Attacker (Attacker,"MeleeRange")) |> opt
                  model.SelectedDefender |> Option.bind(UnitList.View.rootRanges model.Defender (Defender,"MeleeRange")) |> opt
                  model.SelectedAttacker |> Option.bind(UnitList.View.rootRanges model.Attacker (Attacker,"ShootingRange")) |> opt
                  model.SelectedDefender |> Option.bind(UnitList.View.rootRanges model.Defender (Defender,"ShootingRange")) |> opt
                  UnitList.View.root model.Attacker (State.attackerMap >> dispatch)
                  UnitList.View.root model.Defender (State.defenderMap >> dispatch) ] ] 
    let swap =  i [ClassName "column fa fa-arrows-v"; OnClick (fun _ -> Swap |> dispatch) ] []
    let selected = 
        let titleBar text = 
            (section [ ClassName "hero is-primary  has-text-centered"]
                       [ div [ClassName "hero-body"]
                               [ h1 [ClassName "title"]
                                      [ str text] ] ])
        let bar text = 
            (section [ ClassName "hero has-text-centered"]
                       [ div [ClassName "hero-body"]
                               [ h1 [ClassName "title"]
                                      [ str text ] ] ])
        let columnsOf f items =                                       
            let toColumns (left,right) = 
                [left;right]
                |> List.map (List.map snd >> div [ClassName "column"])
                |> div [ClassName "columns"] 
            items
                 |> List.mapi (fun i m -> i,f m)
                 |> List.partition (fun (i,_) -> i % 2 = 0)
                 |> toColumns                
        //let sequence = model.Selected |> Option.map (fun selected -> div [] [ str selected.name ])
        match model.SelectedAttacker |> Option.bind (fun name -> Map.tryFind name model.Attacker.Models) with 
        | None ->  titleBar "<< Select model to edit turn sequence >>"
        | Some selected -> 
             let title = titleBar selected.Name
             let (attrs,actions) = 
                 selected.Attributes 
                 |> Map.partition (fun _ -> snd >> isCharacteristic)
                 |> fun (x,y) -> Map.toList x, Map.toList y
                 |> fun (x,y) -> (List.sortBy (snd >> fst) x), (List.sortBy (snd >> fst) y)
             let attrDiv = 
                 attrs  
                 |> List.map (fun (key,(_,op)) -> MathHammer.Models.View.showAttributes (key,op) dispatch)
                 |> div [ClassName "columns"]  
             let evaluatedActions = actions |> List.choose (fun (name,_) -> Map.tryFind (Attacker,name) selected.Environment |> Option.map(fun dist -> name,dist))
             let actionsDiv = columnsOf (MathHammer.Models.View.showActions dispatch) actions 
             let averagesDiv = columnsOf Probability.View.showAverages evaluatedActions
             let probabiltiesActionsDiv = columnsOf Probability.View.showProbabilitiesOfActions evaluatedActions
             let sampleActionsDiv = columnsOf Probability.View.showSample evaluatedActions

             section [Id "selected"] [ title; attrDiv
                                       bar "Actions"; actionsDiv
                                       bar "Averages"; averagesDiv
                                       bar "Probabilities"; probabiltiesActionsDiv
                                       bar "Sample"; sampleActionsDiv ]
    div [] [ swap; drawing; selected  ] 