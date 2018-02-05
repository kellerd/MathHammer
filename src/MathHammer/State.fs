module MathHammer.State

open Elmish
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.State
let bindModelToEnvironment initial key = 
    Option.map (fun (m : Models.Types.Model) -> 
        let evaluatedRule = 
            m.Rules 
            |> normalize
            |>  evalOp Models.State.standardCall initial
        Map.add key evaluatedRule initial)
let init () : Model * Cmd<Types.Msg> =
    let (attacker,attackerCmd) = UnitList.State.init attackerMap () 
    let (defender,defenderCmd) = UnitList.State.init defenderMap () 
    
    let model : Model = 
        
        { 
            Environment = Map.empty<_,_>
            Attacker = { attacker with BoxFill="#FFCCCC"; ElementFill="#79CE0B"; ElementStroke="#396302"; OffsetY = ft.ToMM 2<ft>; Scale="scale(1,-1)" }
            Defender = { defender with BoxFill="#CCCCFF"; ElementFill="#0B79CE"; ElementStroke="#023963" }
            SelectedAttacker = None
            SelectedDefender = None
            Board = 6<ft>,4<ft>
            GlobalOperations = Map.empty<_,_>
        }
    model, Cmd.batch [ Cmd.map (fun msg -> UnitListMsg(msg, Some attackerMap)) attackerCmd
                       Cmd.map (fun msg -> UnitListMsg(msg, Some defenderMap)) defenderCmd
                       Cmd.ofMsg RebindEnvironment ]

let update msg model : Model * Cmd<Types.Msg> =
    let rebind environment source nameOpt = 
        Option.map (fun name -> 
            let rebind = MathHammer.Models.Types.Msg.Rebind environment
            let mmsg = UnitList.Types.ModelMsg(rebind, name)
            Cmd.ofMsg (UnitListMsg(mmsg, source))
        ) nameOpt
            
    match msg with
    | UnitListMsg (UnitList.Types.ModelMsg(Models.Types.Msg.Select, m), Some "Attacker") -> 
        {model with SelectedAttacker = Some m}, Cmd.batch [ Cmd.ofMsg BindAttacker ]
    | UnitListMsg (UnitList.Types.ModelMsg(Models.Types.Msg.Select, m), Some "Defender") -> 
        {model with SelectedDefender = Some m}, Cmd.batch [ Cmd.ofMsg BindDefender
                                                            Cmd.ofMsg BindAttacker ]
    | UnitListMsg (msg, Some "Attacker")-> 
        let (ula,ulCmdsa) = UnitList.State.update msg model.Attacker
        { model with Attacker = ula }, Cmd.batch [ Cmd.map (fun msg -> UnitListMsg(msg, Some "Attacker")) ulCmdsa]
    | UnitListMsg (msg, Some "Defender")-> 
        let (uld,ulCmdsd) = UnitList.State.update msg model.Defender
        { model with Defender = uld }, Cmd.batch [ Cmd.map (fun msg -> UnitListMsg(msg, Some "Defender")) ulCmdsd]
    | UnitListMsg (_, Some _)-> failwith "No list of that name"
    | UnitListMsg (msg, None) -> 
        let (ula,ulCmdsa) = UnitList.State.update msg model.Attacker
        let (uld,ulCmdsd) = UnitList.State.update msg model.Defender
        { model with Attacker = ula; Defender = uld }, Cmd.batch [ Cmd.map (fun msg -> UnitListMsg(msg, Some attackerMap)) ulCmdsa
                                                                   Cmd.map (fun msg -> UnitListMsg(msg, Some defenderMap)) ulCmdsd ]
    | Swap -> { model with Attacker = { model.Attacker with Models = model.Defender.Models}    
                           Defender = { model.Defender with Models = model.Attacker.Models} 
                           SelectedAttacker = None
                           SelectedDefender = None }, 
                           Cmd.batch [ Cmd.ofMsg ((fun msg -> UnitListMsg(msg, None)) UnitList.Types.Distribute)
                                       Cmd.ofMsg RebindEnvironment ]
    | RebindEnvironment ->      
        let environment = 
            let initial = Map.empty<_,_>
            model.GlobalOperations 
            |> Map.map(fun _ (_,op) -> op 
                                         |> normalize
                                         |>  evalOp Models.State.standardCall initial) 
        {model with Environment = environment}, Cmd.batch [ Cmd.ofMsg BindDefender
                                                            Cmd.ofMsg BindAttacker ]
    | BindDefender -> 
        match model.SelectedDefender with 
        | None -> model, Cmd.none
        | Some defender -> 
            let newEnvironment = 
                Map.tryFind defender model.Defender.Models 
                |> bindModelToEnvironment model.Environment defenderMap 
            match newEnvironment with 
            | Some environment -> 
                let commands = 
                    [ rebind environment (Some defenderMap) model.SelectedDefender
                      rebind environment (Some attackerMap) model.SelectedAttacker ] 
                    |> List.choose id
                    |> Cmd.batch
                {model with Environment = environment}, commands
            | None -> model, Cmd.none
    | BindAttacker -> 
        match model.SelectedAttacker with 
        | None -> model, Cmd.none
        | Some attacker -> 
            let newEnvironment = 
                Map.tryFind attacker model.Attacker.Models 
                |> bindModelToEnvironment model.Environment attackerMap
            match newEnvironment with 
            | Some environment -> 
                let commands = 
                    [ rebind environment (Some defenderMap) model.SelectedDefender
                      rebind environment (Some attackerMap) model.SelectedAttacker ] 
                    |> List.choose id
                    |> Cmd.batch
                {model with Environment = environment}, commands
            | None -> model, Cmd.none


