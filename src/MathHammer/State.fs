module MathHammer.State

open Elmish
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.State
let bindModelToEnvironment initial key = 
    Option.map (fun (m : Models.Types.Model) -> 
        let (choices,normalizedRule) = m.Rules |> normalize
        let evaluatedRule = normalizedRule |> evalOp initial
        choices, Map.add key evaluatedRule initial)
let init () : Model * Cmd<Types.Msg> =
    let (attacker,attackerCmd) = UnitList.State.init attackerMap () 
    let (defender,defenderCmd) = UnitList.State.init defenderMap () 
    
    let model : Model = 
        { 
            Environment = Map.empty<_,_> |> Map.add "Phase" (Str "Assault" |> Value)
            Attacker = { attacker with BoxFill="#FFCCCC"; ElementFill="#79CE0B"; ElementStroke="#396302"; OffsetY = ft.ToMM 2<ft>; Scale="scale(1,-1)" }
            Defender = { defender with BoxFill="#CCCCFF"; ElementFill="#0B79CE"; ElementStroke="#023963" }
            SelectedAttacker = None
            SelectedDefender = None
            Board = {Top = 0<ft>;Left=0<ft>; Width=6<ft>;Height=4<ft>}
            GlobalOperations = []
            Mode = Probability
            Choices = Map.empty<_,_>
            SelectedChoices = Map.empty<_,_>
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
    | UnitListMsg (UnitList.Types.ModelMsg(Models.Types.Msg.MakeChoice(key, options), _), _) -> { model with Choices = Map.mergeSet model.Choices key options }, []
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
    | Choose (key,value) -> 
        { model with SelectedChoices = Map.add key value model.SelectedChoices }, Cmd.ofMsg RebindEnvironment
    | UnitListMsg (_, Some _)-> failwith "No list of that name"
    | UnitListMsg (msg, None) -> 
        let (ula,ulCmdsa) = UnitList.State.update msg model.Attacker
        let (uld,ulCmdsd) = UnitList.State.update msg model.Defender
        { model with Attacker = ula; Defender = uld }, Cmd.batch [ Cmd.map (fun msg -> UnitListMsg(msg, Some attackerMap)) ulCmdsa
                                                                   Cmd.map (fun msg -> UnitListMsg(msg, Some defenderMap)) ulCmdsd ]
    | ChangeMode mode -> {model with Mode = mode}, Cmd.none
    | Swap -> { model with Attacker = { model.Attacker with Models = model.Defender.Models}    
                           Defender = { model.Defender with Models = model.Attacker.Models} 
                           SelectedAttacker = None
                           SelectedDefender = None }, 
                           Cmd.batch [ Cmd.ofMsg ((fun msg -> UnitListMsg(msg, None)) UnitList.Types.Distribute)
                                       Cmd.ofMsg RebindEnvironment ]
    | RebindEnvironment -> 
        let choices = 
            model.SelectedChoices 
            |> Map.map (fun _ value -> Set.singleton value)
        let initial = 
            model.SelectedChoices
            |> Map.map (fun _ value -> Value(Str value))            
        let (choices, environment) = 
            model.GlobalOperations
            |> List.sortBy (snd >> fst)
            |> List.fold(fun (choices,env) (key,(_,op))-> 
                    let (newChoices,normal) = op |> normalize 
                    let result = normal |> evalOp env
                    Map.mergeSets choices newChoices, Map.add key result env) (choices, initial)   
        let cmds = Cmd.batch [ Cmd.ofMsg BindDefender; Cmd.ofMsg BindAttacker ]            
        { model with Environment = environment; Choices = choices }, cmds  
    | BindDefender -> 
        match model.SelectedDefender with 
        | None -> model, Cmd.none
        | Some defender -> 
            let newEnvironment = 
                Map.tryFind defender model.Defender.Models 
                |> bindModelToEnvironment model.Environment defenderMap 
            match newEnvironment with 
            | Some (choices,environment) -> 
                let cmds = 
                    [ rebind environment (Some defenderMap) model.SelectedDefender
                      rebind environment (Some attackerMap) model.SelectedAttacker ] 
                    |> List.choose id
                    |> Cmd.batch
                { model with Environment = environment; Choices = Map.mergeSets choices model.Choices }, cmds
            | None -> model, Cmd.none
    | BindAttacker -> 
        match model.SelectedAttacker with 
        | None -> model, Cmd.none
        | Some attacker -> 
            let newEnvironment = 
                Map.tryFind attacker model.Attacker.Models 
                |> bindModelToEnvironment model.Environment attackerMap
            match newEnvironment with 
            | Some (choices,environment) -> 
                let cmds = 
                    [ rebind environment (Some defenderMap) model.SelectedDefender
                      rebind environment (Some attackerMap) model.SelectedAttacker ] 
                    |> List.choose id
                    |> Cmd.batch
                { model with Environment = environment; Choices = Map.mergeSets choices model.Choices }, cmds
            | None -> model, Cmd.none


