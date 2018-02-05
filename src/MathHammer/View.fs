module MathHammer.View

open Fable.Helpers.React
open Props
open Types
open GameActions.Primitives.Types
open Models.View  
open GameActions.Primitives.State
let root model dispatch =
    let (boardX,boardY) = model.Board |> fun (x,y) -> ft.ToMM(x),ft.ToMM(y)
    let drawing =   
        div [] 
            [svg 
                [ ViewBox (sprintf "0 0 %d %d" boardX boardY); unbox ("width", "100%")]
                [ UnitList.View.rootBoard model.Attacker (fun msg -> UnitListMsg(msg, Some attackerMap) |> dispatch)
                  UnitList.View.rootBoard model.Defender (fun msg -> UnitListMsg(msg, Some defenderMap) |> dispatch)
                  model.SelectedAttacker |> Option.bind(UnitList.View.rootRanges model.Attacker ("MeleeRange")) |> opt
                  model.SelectedDefender |> Option.bind(UnitList.View.rootRanges model.Defender ("MeleeRange")) |> opt
                  model.SelectedAttacker |> Option.bind(UnitList.View.rootRanges model.Attacker ("ShootingRange")) |> opt
                  model.SelectedDefender |> Option.bind(UnitList.View.rootRanges model.Defender ("ShootingRange")) |> opt
                  UnitList.View.root model.Attacker (fun msg -> UnitListMsg(msg, Some attackerMap) |> dispatch)
                  UnitList.View.root model.Defender (fun msg -> UnitListMsg(msg, Some defenderMap) |> dispatch) ] ] 
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
            let getListOfOps = function | Value(ParamArray ops) -> ops | op -> [op]
            let actionsDiv = selected.NormalizedRules |> showActions dispatch |> div []
            let averagesDiv = selected.ProbabilityRules |> getListOfOps |> List.collect (showAverages dispatch) |> columnsOf
            let probabiltiesActionsDiv = selected.ProbabilityRules |> getListOfOps |> List.collect (showActions dispatch) |> columnsOf
            let sampleActionsDiv = selected.ProbabilityRules |> getListOfOps |> List.collect (showSamples dispatch) |> columnsOf

            section [Id "selected"] [ title; attrDiv
                                      bar "Actions"; actionsDiv
                                      bar "Averages"; averagesDiv
                                      bar "Probabilities"; probabiltiesActionsDiv
                                      bar "Sample"; sampleActionsDiv ]
            
    div [] [ swap; drawing; selected  ] 