module MathHammer.View

open Fable.Helpers.React
open Props
open Types
open GameActions.Primitives.Types
open Models.View
open GameActions.Primitives.State

let line colour (x1, y1) (x2, y2) tension width =
    let (hx1, hy1, hx2, hy2) =
        let delta =
            if tension < 0.0 then (y2 - y1) * tension
            else (x2 - x1) * tension
        if tension < 0.0 then x1, y1 - delta, x2, y2 + delta
        else x1 + delta, y1, x2 - delta, y2
    
    let pathString =
        sprintf "M %f %f C %f %f %f %f %f %f" x1 y1 hx1 hy1 hx2 hy2 x2 y2
    path [ SVGAttr.D pathString
           SVGAttr.Fill "none"
           SVGAttr.Stroke colour
           SVGAttr.StrokeWidth width
           SVGAttr.MarkerEnd "url(#arrow)"
           SVGAttr.MarkerStart "url(#arrowBack)" ] []

// let arrowLine  
let arrowDef =
    let marker b c = svgEl "marker" b c
    [ marker [ SVGAttr.Custom("markerWidth", "50")
               SVGAttr.Custom("markerHeight", "50")
               SVGAttr.Custom("refX", "25")
               SVGAttr.Custom("refY", "10")
               SVGAttr.Custom("orient", "auto")
               SVGAttr.Custom("markerUnits", "userSpaceOnUse")
               Id "arrow" ] [ path [ D "M0,0 L0,20 L25,10 z"
                                     SVGAttr.Fill "green" ] [] ]
      marker [ SVGAttr.Custom("markerWidth", "50")
               SVGAttr.Custom("markerHeight", "50")
               SVGAttr.Custom("refX", "25")
               SVGAttr.Custom("refY", "10")
               SVGAttr.Custom("orient", "auto")
               SVGAttr.Custom("markerUnits", "userSpaceOnUse")
               Id "arrowBack" ] [ path [ SVGAttr.Transform "rotate(180 25,10)"
                                         D "M25,10 L0,20 L0,0 z"
                                         SVGAttr.Fill "green" ] [] ] ]
    |> defs []

let root model dispatch =
    let viewbox =
        ViewBox
            (sprintf "%d %d %d %d" model.Board.Top model.Board.Left 
                 model.Board.Width model.Board.Height)

    let selectedAttacker =
        model.SelectedAttacker 
        |> Option.bind (fun name -> Map.tryFind name model.Attacker.Models)
    let selectedLines =
        selectedAttacker         
        |> Option.toList
        |> List.collect (fun a -> 
            [ for d in model.Defender.Models do
                let matchup = Map.find {Attacker = a.Name; Defender = d.Key} model.Matchups
                let environment = Map.add defenderMap d.Value.Rules model.Environment
                let ap = (a.PosX, a.PosY)
                let dp = (d.Value.PosX, d.Value.PosY)
                let log f = printfn "%A" f; f
                let unsavedWounds = 
                    match (getp "Unsaved Wounds" matchup |> evalOp environment ) |> log with 
                    | Value (v) -> 
                        match v with 
                        | Int _         
                        | Float _       -> Some v
                        | Str _         -> None
                        | Check _       -> None
                        | NoValue       -> failwith "Not Implemented"
                        | ParamArray _  -> failwith "Not Implemented"
                        | Tuple(Check (Check.Pass v), Check (Check.Fail _)) -> Some (Check (Check.Pass v))                      
                        | Tuple _       -> None                        
                        | Dist(d)       -> d.Probabilities |> List.sumBy (fun (a, p) -> a * (Float p)) |> Some
                    | Call _                                  -> None
                    | PropertyGet _                     -> None
                    | Var _                                 -> None
                    | App _                           -> None
                    | Lam _                      -> None
                    | Let _                    -> None
                    | IfThenElse _   -> None
                    | Choice _               -> None                        
                    |> Option.map (function 
                                       | Check(Check.Pass(Int(wounds))) -> float wounds
                                       | Tuple(Check (Check.Pass (Float(wounds))), Check (Check.Fail _)) -> wounds
                                       | Tuple(Check (Check.Pass (Int(wounds))), Check (Check.Fail _)) -> float wounds
                                       | Check(Check.Pass(Float(wounds))) -> wounds
                                       | Int(wounds) -> float wounds 
                                       | Float(wounds) -> wounds 
                                       | v -> log v |> ignore; 0.0
                                    >> (*) 25.0
                                    >> line "green" ap dp -0.5 )    
                yield unsavedWounds ] )
        |> List.choose id        
    
    let drawing =
        svg [ viewbox
              unbox ("width", "100%") ] [ //arrowDef
                                          
                                          [ model.Defender.Location
                                            model.Attacker.Location
                                            model.Defender.Deployment
                                            model.Attacker.Deployment ]
                                          |> List.map UnitList.View.rootLocation 
                                          |> ofList    

                                          model.SelectedAttacker
                                          |> Option.bind 
                                                 (UnitList.View.rootRanges 
                                                      model.Attacker 
                                                      ("Shooting Range"))
                                          |> ofOption
                                          model.SelectedDefender
                                          |> Option.bind 
                                                 (UnitList.View.rootRanges 
                                                      model.Defender 
                                                      ("Shooting Range"))
                                          |> ofOption
                                          model.SelectedDefender
                                          |> Option.bind 
                                                 (UnitList.View.rootRanges 
                                                      model.Defender 
                                                      ("Assault Range"))
                                          |> ofOption
                                          model.SelectedAttacker
                                          |> Option.bind 
                                                 (UnitList.View.rootRanges 
                                                      model.Attacker 
                                                      ("Assault Range"))
                                          |> ofOption
                                          
                                          UnitList.View.root model.Attacker 
                                              (fun msg -> 
                                              UnitListMsg(msg, Some attackerMap) 
                                              |> dispatch)
                                          
                                          UnitList.View.root model.Defender 
                                              (fun msg -> 
                                              UnitListMsg(msg, Some defenderMap) 
                                              |> dispatch)

                                          selectedLines
                                          |> ofList ]
    
    let swap =
        i [ ClassName "fa fa-arrows-v"
            OnClick(fun _ -> Swap |> dispatch) ] []
        |> List.singleton
        |> div [ ClassName "column has-text-centered " ]
    
    let selected =
        let titleBar text =
            (section [ ClassName "hero is-primary  has-text-centered" ] 
                 [ div [ ClassName "hero-body" ] 
                       [ h1 [ ClassName "title" ] [ str text ] ] ])
        let bar text =
            (section [ ClassName "hero has-text-centered" ] 
                 [ div [ ClassName "hero-body" ] 
                       [ h1 [ ClassName "title" ] [ str text ] ] ])
        
        let columnsOf items =
            let toColumns (left, right) =
                [ left; right ]
                |> List.map (List.map snd >> div [ ClassName "column" ])
                |> div [ ClassName "columns" ]
            items
            |> List.mapi (fun i m -> i, m)
            |> List.partition (fun (i, _) -> i % 2 = 0)
            |> toColumns
        match selectedAttacker with
        | None -> titleBar "<< Select model to edit turn sequence >>"
        | Some selected -> 
            let attrDiv =
                selected.Attributes
                |> Map.toList
                |> List.sortBy (fun (_, (ord, _)) -> ord)
                |> List.map 
                       (fun (key, (_, op)) -> showAttributes (key, op) dispatch)
                |> div [ ClassName "columns" ]
            
            let getListOfOps =
                function 
                | Value(ParamArray ops) -> ops
                | op -> [ op ]
            
            let (resultName, showFunction) =
                match model.Mode with
                | Average -> "Averages", showAverages
                | Probability -> "Probabilities", showActions
                | Sample -> "Sample", showSamples
            
            let menuItem mode =
                let isActive =
                    if model.Mode = mode then 
                        [ ClassName "is-active" :> IHTMLProp ]
                    else []
                li isActive 
                    [ a [ OnClick(fun _ -> dispatch (ChangeMode mode)) ] 
                          [ str (mode.ToString()) ] ]
            
            let resultsDiv =
                selected.ProbabilityRules
                |> Option.map (getListOfOps
                               >> List.map (showFunction dispatch)
                               >> columnsOf)
                |> ofOption
            
            let inline chunkBySize size =
                Seq.ofList
                >> Seq.chunkBySize size
                >> Seq.map (Array.toList)
                >> Seq.toList
            
            let choices =
                model.Choices
                |> Map.toList
                |> List.map (fun (name, choices) -> 
                       let radioButtons =
                           choices
                           |> Set.toList
                           |> List.map (fun choice -> 
                                  Fable.Helpers.React.label [] [ input 
                                                                     [ Name name
                                                                       
                                                                       Type 
                                                                           "radio"
                                                                       
                                                                       Props.HTMLAttr.Value 
                                                                           choice
                                                                       
                                                                       Checked
                                                                           (Map.tryFind 
                                                                                name 
                                                                                model.SelectedChoices
                                                                            |> Option.map 
                                                                                   ((=) 
                                                                                        choice)
                                                                            |> Option.defaultValue 
                                                                                   false)
                                                                       
                                                                       OnClick
                                                                           (fun _ -> 
                                                                           Choose
                                                                               (name, 
                                                                                choice) 
                                                                           |> dispatch) ]
                                                                 str choice
                                                                 br [] ])
                       article [ ClassName "tile is-child box" ] 
                           (p [ ClassName "title" ] [ str name ] :: radioButtons)
                       |> List.singleton
                       |> div [ ClassName "tile is-parent" ])
                |> chunkBySize 4
                |> List.map (function 
                       | [] -> ofOption None
                       | xs -> div [ ClassName "tile is-ancestor" ] xs)
                |> section [ Id "choices" ]
            
            section [ Id "selected" ] [ titleBar selected.Name
                                        bar "Profile"
                                        attrDiv
                                        choices
                                        
                                        div 
                                            [ ClassName 
                                                  "tabs is-fullwidth is-toggle is-toggle-rounded" ] 
                                            [ ul [] [ menuItem Average
                                                      menuItem Probability
                                                      menuItem Sample ] ]
                                        bar resultName
                                        resultsDiv ]
    
    ofList [ swap; drawing; selected ]
