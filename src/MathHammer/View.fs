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
        svg 
            [ ViewBox (sprintf "0 0 %d %d" boardX boardY); unbox ("width", "100%")]
            [ UnitList.View.rootBoard model.Attacker (fun msg -> UnitListMsg(msg, Some attackerMap) |> dispatch)
              UnitList.View.rootBoard model.Defender (fun msg -> UnitListMsg(msg, Some defenderMap) |> dispatch)
              model.SelectedAttacker |> Option.bind(UnitList.View.rootRanges model.Attacker ("Shooting Range")) |> ofOption
              model.SelectedDefender |> Option.bind(UnitList.View.rootRanges model.Defender ("Shooting Range")) |> ofOption
              model.SelectedDefender |> Option.bind(UnitList.View.rootRanges model.Defender ("Assault Range")) |> ofOption
              model.SelectedAttacker |> Option.bind(UnitList.View.rootRanges model.Attacker ("Assault Range")) |> ofOption
              UnitList.View.root model.Attacker (fun msg -> UnitListMsg(msg, Some attackerMap) |> dispatch)
              UnitList.View.root model.Defender (fun msg -> UnitListMsg(msg, Some defenderMap) |> dispatch) ] 
    let swap =  
        i [ClassName "fa fa-arrows-v"; OnClick (fun _ -> Swap |> dispatch) ] []
        |> List.singleton
        |> div [ClassName "column has-text-centered "]
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
        
        match model.SelectedAttacker |> Option.bind (fun name -> Map.tryFind name model.Attacker.Models) with 
        | None ->  
            titleBar "<< Select model to edit turn sequence >>"
        | Some selected -> 
            let attrDiv = 
                selected.Attributes
                |> Map.toList  
                |> List.sortBy (fun (_,(ord,_)) -> ord)
                |> List.map    (fun (key,(_,op)) -> showAttributes (key,op) dispatch)
                |> div [ClassName "columns"]  
            let getListOfOps = function | Value(ParamArray ops) -> ops | op -> [op]
            let (resultName, showFunction) = 
                match model.Mode with
                | Average     -> "Averages"     , showAverages
                | Probability -> "Probabilities", showActions 
                | Sample      -> "Sample"       , showSamples
            let menuItem mode = 
                let isActive = if model.Mode = mode then [ClassName "is-active" :> IHTMLProp] else []
                li isActive [a [OnClick (fun _ -> dispatch (ChangeMode mode))]   [str (mode.ToString())     ]]
               
            let resultsDiv = selected.ProbabilityRules |> getListOfOps |> List.collect (showFunction dispatch)  |> columnsOf
              
            let inline chunkBySize size = Seq.ofList >> Seq.chunkBySize size >> Seq.map (Array.toList) >> Seq.toList

            let choices = 
                model.Choices
                |> Map.toList
                |> List.map (fun (name, choices) ->
                    let radioButtons = 
                        choices 
                        |> Set.toList
                        |> List.map (fun choice ->
                            Fable.Helpers.React.label 
                                [] 
                                [ input [ Name name
                                          Type "radio"
                                          Props.HTMLAttr.Value choice
                                          Checked (Map.tryFind name model.SelectedChoices |> Option.map((=) choice) |> Option.defaultValue false)
                                          OnClick (fun _ -> Choose(name,choice) |> dispatch) ]
                                  str choice
                                  br [] ] )

                    article [ ClassName "tile is-child box"] 
                            (p [ClassName "title"] [str name] :: radioButtons) 
                    |> List.singleton
                    |> div [ ClassName "tile is-parent"] )
                |> chunkBySize 4
                |> List.map (function [] -> ofOption None | xs ->  div [ClassName "tile is-ancestor"] xs)
                |> section [Id "choices"]
            section [Id "selected"] [ titleBar  selected.Name 
                                      bar       "Profile" 
                                      attrDiv
                                      choices
                                      div [ClassName "tabs is-fullwidth is-toggle is-toggle-rounded"] [
                                          ul [] [
                                              menuItem Average   
                                              menuItem Probability
                                              menuItem Sample   
                                          ] 
                                      ]
                                      bar resultName; resultsDiv ]

    ofList [ swap; drawing; selected ] 