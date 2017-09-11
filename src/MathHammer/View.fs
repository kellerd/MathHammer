module MathHammer.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish.React
open MathHammer.Models.Types
open Types
open GameActions.Primitives.Types
open MathHammer.Models.View  
let root model dispatch =
    let (boardX,boardY) = model.Board |> fun (x,y) -> ft.ToMM(x),ft.ToMM(y)
    let drawing =   
        div [] 
            [svg 
                [ ViewBox (sprintf "0 0 %d %d" boardX boardY); unbox ("width", "100%")]
                [ UnitList.View.rootBoard model.Attacker (State.attackerMap >> dispatch)
                  UnitList.View.rootBoard model.Defender (State.defenderMap >> dispatch)
                  model.SelectedAttacker |> Option.bind(UnitList.View.rootRanges model.Attacker ("MeleeRange")) |> opt
                  model.SelectedDefender |> Option.bind(UnitList.View.rootRanges model.Defender ("MeleeRange")) |> opt
                  model.SelectedAttacker |> Option.bind(UnitList.View.rootRanges model.Attacker ("ShootingRange")) |> opt
                  model.SelectedDefender |> Option.bind(UnitList.View.rootRanges model.Defender ("ShootingRange")) |> opt
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
        let columnsOf items =                                       
            let toColumns (left,right) = 
                [left;right]
                |> List.map (List.map snd >> div [ClassName "column"])
                |> div [ClassName "columns"] 
            items
                 |> List.mapi (fun i m -> i,m)
                 |> List.partition (fun (i,_) -> i % 2 = 0)
                 |> toColumns                
        //let sequence = model.Selected |> Option.map (fun selected -> div [] [ str selected.name ])
        
        match model.SelectedAttacker |> Option.bind (fun name -> Map.tryFind name model.Attacker.Models) with 
        | None ->  
            titleBar "<< Select model to edit turn sequence >>"
        | Some selected -> 
            let title = titleBar selected.Name
            let rec (|LabeledParams|) args op =
                match args with 
                | [] -> []
                | x :: xs ->
                    match op with 
                    | Lam(var, LabeledParams xs lambda) -> (var,x) :: lambda
                    | _ -> []
            let (LabeledParams selected.Attributes attrs) = selected.Rules
            let rec attrDiv = 
                attrs  
                |> List.map (fun (key,op) -> showAttributes (key,op) dispatch)
                |> div [ClassName "columns"]  
            let getListOfOps = function | ParamArray(OpList(ops)) -> ops | op -> [op]
            let actionsDiv = selected.Rules |> showActions dispatch  |> columnsOf 
            let averagesDiv = selected.AverageRules |> getListOfOps |> List.map (showOperationDistribution showProbabilitiesOfActions)  |> columnsOf
            let probabiltiesActionsDiv = selected.ProbabilityRules |> getListOfOps |> List.map (showOperationDistribution showProbabilitiesOfActions) |> columnsOf
            let sampleActionsDiv = selected.SampleRules |> getListOfOps |> List.map (showOperationDistribution showProbabilitiesOfActions) |> columnsOf

            section [Id "selected"] [ title; attrDiv
                                      bar "Actions"; actionsDiv
                                      bar "Averages"; averagesDiv
                                      bar "Probabilities"; probabiltiesActionsDiv
                                      bar "Sample"; sampleActionsDiv ]
            
    div [] [ swap; drawing; selected  ] 