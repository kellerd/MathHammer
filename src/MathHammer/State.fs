module MathHammer.State

open Elmish
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.Units
open GameActions.Primitives.State
open MathHammer.Models.State
open MathHammer.UnitList.State
open Fable

let range = vInt

let lam = Lam
               ("Target",
                Let
                  ("roll",
                   Value
                     (ParamArray
                        [App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [ App (Call Dice,Value (Int 6));Value(Int 1)]))]),
                   Value
                     (ParamArray
                        [IfThenElse
                           (Value
                              (ParamArray
                                 [App
                                    (Call Repeat,
                                     Value
                                       (ParamArray
                                          [Lam
                                             ("roll",
                                              Let
                                                ("gt",
                                                 App
                                                   (Call GreaterThan,
                                                    Value
                                                      (ParamArray
                                                         [Var "roll";
                                                          Value (Int 4)])),
                                                 Let
                                                   ("eq",
                                                    App
                                                      (Call Equals,
                                                       Value
                                                         (ParamArray
                                                            [Var "roll";
                                                             Value (Int 4)])),
                                                    App
                                                      (Call Or,
                                                       Value
                                                         (ParamArray
                                                            [Var "eq";
                                                             Var "gt"])))));Value (Int 1)]))]),
                            Value
                              (ParamArray
                                 [ Let("W", PropertyGet("W", Var "Target"), App(Call Total, Value(ParamArray [Var "W"; Value(Int(-7))])))]),
                            None)])));
let op = Var "Defender"                            
let suffers = App (Call FMap, Value (ParamArray [lam; Var "Defender"]))

let phaseActions =
    choose "Phase" [ "Assault", 
                     nestOps 
                         [ get "Assault Range" <*> get "M" 
                           <*> get "Charge Range" >>= "Assault Range"
                           
                           get "Range Check" <*> get "Range" 
                           <*> get "Assault Range" >>= "Can Hit"
                           
                           get "To Hit" <*> get "WS" <*> get "A" 
                           <*> get "Can Hit" >>= "Hit Results"
                           
                           // (get "Strength vs Toughness Table" <*> get "S" <*> get "Defender") >>= "Wound Results"                  
                           get "To Wound" <*> get "Hit Results" 
                           <*> (get "Strength vs Toughness Table" 
                                <*> get "Defender" <*> get "S") 
                           >>= "Wound Results"
                           get "App 2 Test" >>= "App2"
                           
                           get "Armour Save" <*> get "Defender" 
                           <*> get "Wound Results" >>= "Unsaved Wounds"
                        
                           suffers >>= "Suffers" 
                           ]
                     <| opList [ labelVar "Charge Range"
                                 labelVar "Assault Range"
                                 labelVar "Can Hit"
                                 labelVar "Hit Results"
                                 labelVar "Wound Results"
                                 labelVar "App2"
                                 labelVar "Unsaved Wounds"
                                 labelVar "Suffers" ]
                     "Shooting", labelVar "Shooting Range"
                     "Psychic", labelVar "Psychic Test" ]
    >>= "Actions"

let dPhaseActions =
    choose "Phase" [ "Assault", 
                     (choose "Weapon" [ "Bolter", range 24
                                        "Melta", range 12 ]
                      >>= "Weapon Range") (labelVar "Shooting Range")
                     "Psychic", labelVar "Deny Test" ]
    >>= "Actions"

let allPropsa =
    opList [ //  labelVar "D6Test"
             //  labelVar "D3Test"
             labelProp "Actions" "Can Hit"
             labelProp "Actions" "Hit Results"
             labelProp "Actions" "Wound Results"
             // labelProp "Actions" "App2"
             labelProp "Actions" "Unsaved Wounds"
             labelProp "Actions" "Charge Range"
             labelProp "Actions" "Assault Range"
             labelProp "Actions" "Psychic Test"
             labelProp "Actions" "Shooting Range"
             labelProp "Actions" "Suffers"
             labelVar "M"
             labelVar "WS"
             labelVar "BS"
             labelVar "S"
             labelVar "T"
             labelVar "W"
             labelVar "A"
             labelVar "Ld"
             labelVar "Sv"
             labelVar "InvSv" ]

let allPropsd =
    opList [ labelVar "M"
             labelVar "WS"
             labelVar "BS"
             labelVar "S"
             labelVar "T"
             labelVar "W"
             labelVar "A"
             labelVar "Ld"
             labelVar "Sv"
             labelVar "InvSv"
             labelVar "D6Test"
             labelVar "D3Test"
             labelProp "Actions" "Shooting Range"
             labelProp "Actions" "Deny Test" ]

let init() : Model * Cmd<Types.Msg> =
    let (attacker, attackerCmd) = UnitList.State.init None ()
    let (defender, defenderCmd) = UnitList.State.init None ()
    let body = nestOps [ phaseActions ] allPropsa
    let defbody = nestOps [ dPhaseActions ] allPropsd
    let stats = [ "M"; "WS"; "BS"; "S"; "T"; "W"; "A"; "Ld"; "Sv"; "InvSv" ]
    let attackerDefinition = createArgs stats body
    let defenderDefinition = createArgs stats defbody
    
    let (attackerModels, attackerCmds) =
        [ initMeq "Marine" attackerDefinition
          initSgt "Captain" attackerDefinition ]
        |> List.map 
               (fun ((name, model), cmd) -> 
               (name, model), 
               Cmd.map (fun msg -> UnitList.Types.ModelMsg(msg, name)) cmd)
        |> List.unzip
    
    let geq = initGeq "Geq" defenderDefinition
    
    let (defenderModels, modelCmds) =
        [ 'a'..'z' ]
        |> List.mapi (fun i c -> 
               let ((_, geq), modelCmd) = geq i (6 - i % 5)
               let name = sprintf "T%d Sv%d" i (6 - i % 5)
               (name, { geq with Name = name }), modelCmd)
        |> List.unzip
    
    let defenderCmds =
        List.map (Cmd.map (fun msg -> UnitList.Types.ModelMsg(msg, "Geq"))) 
            modelCmds |> Cmd.batch
    
    let model : Model =
        { Dragging = false, None
          Environment =
              Map.empty |> Map.add "Phase" (Str "Assault" |> Value)
          Attacker =
              { attacker with Location =
                                  { attacker.Location with Fill = "#FFCCCC"
                                                           Dimensions =
                                                               { attacker.Location.Dimensions with Top =
                                                                                                       ft.ToMM 
                                                                                                           2<ft> } }
                              ElementFill = "#79CE0B"
                              ElementStroke = "#396302"
                              Models = Map.ofList attackerModels
                              Deployment =
                                  { attacker.Deployment with Dimensions =
                                                                 { attacker.Deployment.Dimensions with Top =
                                                                                                           attacker.Deployment.Dimensions.Top 
                                                                                                           + ft.ToMM 
                                                                                                                 2<ft> 
                                                                                                           + ft.ToMM 
                                                                                                                 1<ft> } } }
          Defender =
              { defender with Location =
                                  { defender.Location with Fill = "#CCCCFF" }
                              ElementFill = "#0B79CE"
                              Models = Map.ofList defenderModels
                              ElementStroke = "#023963" }
          SelectedAttacker = None
          SelectedDefender = None
          Matchups = Map.empty
          Board =
              { Top = 0<mm>
                Left = 0<mm>
                Width = ft.ToMM 6<ft>
                Height = ft.ToMM 4<ft> }
          GlobalOperations = []
          Mode = Probability
          Choices = Map.empty
          SelectedChoices = Map.empty }
    model, 
    Cmd.batch [ List.map 
                    (Cmd.map (fun msg -> UnitListMsg(msg, Some attackerMap))) 
                    (attackerCmd :: attackerCmds) |> Cmd.batch
                
                List.map 
                    (Cmd.map (fun msg -> UnitListMsg(msg, Some defenderMap))) 
                    [ defenderCmd; defenderCmds ] |> Cmd.batch
                
                Cmd.ofMsg 
                    (UnitListMsg
                         (UnitList.Types.Distribute
                              (UnitList.Types.Area 
                                   model.Attacker.Deployment.Dimensions), 
                          Some defenderMap))
                
                Cmd.ofMsg 
                    (UnitListMsg
                         (UnitList.Types.Distribute
                              (UnitList.Types.Area 
                                   model.Defender.Deployment.Dimensions), 
                          Some attackerMap))
                Cmd.ofMsg RebindEnvironment ]

let performMatchup def (attacker : Models.Types.Model) env =
    let (defName, defRules, defRange) =
        match def with
        | Some(d : Models.Types.Model) -> 
            let range =
                distance (d.PosX, d.PosY) (attacker.PosX, attacker.PosY)
                |> abs
                |> (*) 1.<mm>
                |> inch.FromMMf
            Some d.Name, d.Rules, Value(Float(float range))
        | None -> None, Value(NoValue), Value(NoValue)
    
    let initial =
        env
        |> Map.add "Range" defRange
        |> Map.add defenderMap (Value(ParamArray[defRules])) //Make it a unit
    
    let evaluatedRule = attacker.Rules |> evalOp initial
    ({ Defender = defName
       Attacker = attacker.Name }, evaluatedRule)

let update msg model : Model * Cmd<Types.Msg> =
    let rebind environment source nameOpt =
        Option.map (fun name -> 
            let rebind = MathHammer.Models.Types.Msg.Rebind environment
            let mmsg = UnitList.Types.ModelMsg(rebind, name)
            Cmd.ofMsg (UnitListMsg(mmsg, source))) nameOpt
    match msg with
    | EndDrag -> 
        let (_, name) = model.Dragging
        
        let cmds =
            match name with
            | Some(a, _) when a = attackerMap -> Cmd.ofMsg BindAttacker
            | Some(d, _) when d = defenderMap -> Cmd.ofMsg BindDefender
            | _ -> Cmd.none
        { model with Dragging = false, name }, cmds
    | Drag -> 
        let cmds =
            match model.Dragging with
            | true, Some(a, _) when a = attackerMap -> Cmd.ofMsg BindAttacker
            | true, Some(d, _) when d = defenderMap -> Cmd.ofMsg BindDefender
            | _ -> Cmd.none
        model, cmds
    | StartDrag -> 
        let (_, name) = model.Dragging
        { model with Dragging = true, name }, Cmd.none
    | UnitListMsg(UnitList.Types.ModelMsg(Models.Types.Msg.Select, m), 
                  Some "Attacker") -> 
        let (isDragging, _) = model.Dragging
        if not isDragging then 
            { model with SelectedAttacker = Some m
                         Dragging = (false, Some(attackerMap, m)) }, 
            Cmd.batch [ Cmd.ofMsg BindAttacker ]
        else model, Cmd.none
    | UnitListMsg(UnitList.Types.ModelMsg(Models.Types.Msg.Select, m), 
                  Some "Defender") -> 
        let (isDragging, _) = model.Dragging
        if not isDragging then 
            { model with SelectedDefender = Some m
                         Dragging = (false, Some(defenderMap, m)) }, 
            Cmd.batch [ Cmd.ofMsg BindDefender ]
        else model, Cmd.none
    | UnitListMsg(msg, Some "Attacker") -> 
        let (ula, ulCmdsa) = UnitList.State.update msg model.Attacker
        { model with Attacker = ula }, 
        Cmd.batch 
            [ Cmd.map (fun msg -> UnitListMsg(msg, Some "Attacker")) ulCmdsa ]
    | UnitListMsg(msg, Some "Defender") -> 
        let (uld, ulCmdsd) = UnitList.State.update msg model.Defender
        { model with Defender = uld }, 
        Cmd.batch 
            [ Cmd.map (fun msg -> UnitListMsg(msg, Some "Defender")) ulCmdsd ]
    | Choose(key, value) -> 
        { model with SelectedChoices = Map.add key value model.SelectedChoices
                     Environment =
                         Map.add key (Value(Str(value))) model.Environment }, 
        Cmd.batch [ Cmd.ofMsg BindDefender
                    Cmd.ofMsg BindAttacker ]
    | UnitListMsg(_, Some _) -> failwith "No list of that name"
    | UnitListMsg(msg, None) -> 
        let (ula, ulCmdsa) = UnitList.State.update msg model.Attacker
        let (uld, ulCmdsd) = UnitList.State.update msg model.Defender
        { model with Attacker = ula
                     Defender = uld }, 
        Cmd.batch 
            [ Cmd.map (fun msg -> UnitListMsg(msg, Some attackerMap)) ulCmdsa
              Cmd.map (fun msg -> UnitListMsg(msg, Some defenderMap)) ulCmdsd ]
    | ChangeMode mode -> { model with Mode = mode }, Cmd.none
    | Swap -> 
        { model with Attacker =
                         { model.Attacker with Models = model.Defender.Models }
                     Defender =
                         { model.Defender with Models = model.Attacker.Models }
                     SelectedAttacker = None
                     SelectedDefender = None }, 
        Cmd.batch 
            [ Cmd.ofMsg 
                  (UnitListMsg
                       (UnitList.Types.Distribute
                            (UnitList.Types.Area 
                                 model.Attacker.Deployment.Dimensions), 
                        Some defenderMap))
              
              Cmd.ofMsg 
                  (UnitListMsg
                       (UnitList.Types.Distribute
                            (UnitList.Types.Area 
                                 model.Defender.Deployment.Dimensions), 
                        Some attackerMap)) ]
    | RebindEnvironment -> 
        let environment =
            model.GlobalOperations
            |> List.sortBy (snd >> fst)
            |> List.fold (fun env (key, (_, op)) -> 
                   let result = op |> evalOp env
                   Map.add key result env) Map.empty
        
        let allChoices =
            [ model.Attacker.Models
              |> Map.toList
              |> List.map (fun (_, m) -> m.Choices)
              model.Defender.Models
              |> Map.toList
              |> List.map (fun (_, m) -> m.Choices) ]
            |> List.collect id
            |> List.reduceSafe Map.empty (Map.mergeSets)
            |> Map.mergeSets model.Choices
        
        { model with Environment = environment
                     Matchups = Map.empty
                     Choices = allChoices
                     SelectedDefender = None
                     SelectedAttacker = None }, Cmd.none
    | BindDefender -> 
        let models =
            match model.SelectedAttacker with
            | Some key2 -> 
                let matchup =
                    Map.find { Defender = model.SelectedDefender
                               Attacker = key2 } model.Matchups
                Map.map (fun k (m : Models.Types.Model) -> 
                    if k = key2 then { m with ProbabilityRules = Some matchup }
                    else m) model.Attacker.Models
            | _ -> model.Attacker.Models
        
        let newMatchup =
            let d =
                model.SelectedDefender 
                |> Option.map 
                       (fun defender -> Map.find defender model.Defender.Models)
            let a =
                model.SelectedAttacker 
                |> Option.map 
                       (fun attacker -> Map.find attacker model.Attacker.Models)
            match a with
            | Some a -> 
                let (key, matchup) = performMatchup d a model.Environment
                Map.add key matchup model.Matchups
            | _ -> model.Matchups
        
        let defenderModels =
            Map.map (fun k (m : Models.Types.Model) -> 
                match model.SelectedDefender with
                | Some key when key = k -> 
                    { m with ProbabilityRules =
                                 Some(evalOp model.Environment m.Rules) }
                | _ -> m) model.Defender.Models
        
        { model with Attacker = { model.Attacker with Models = models }
                     Defender = { model.Defender with Models = defenderModels }
                     Matchups = newMatchup }, Cmd.none
    | BindAttacker -> 
        let foundAttacker =
            model.SelectedAttacker 
            |> Option.bind 
                   (fun attacker -> Map.tryFind attacker model.Attacker.Models)
        match foundAttacker with
        | None -> model, Cmd.none
        | Some attacker -> 
            let matchups =
                [ yield performMatchup None attacker model.Environment
                  
                  for def in model.Defender.Models do
                      yield performMatchup (Some def.Value) attacker 
                                model.Environment ]
                |> Map.ofList
            
            let models =
                match model.SelectedAttacker with
                | Some key2 -> 
                    let matchup =
                        Map.find { Defender = model.SelectedDefender
                                   Attacker = key2 } matchups
                    Map.map (fun k (m : Models.Types.Model) -> 
                        if k = key2 then 
                            { m with ProbabilityRules = Some matchup }
                        else m) model.Attacker.Models
                | _ -> Map.remove attackerMap model.Attacker.Models
            
            { model with Attacker = { model.Attacker with Models = models }
                         Matchups = matchups }, Cmd.none
