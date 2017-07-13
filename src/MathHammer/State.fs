module MathHammer.State

open Elmish
open Types
open GameActions.Primitives.Types
let attackerMap msg = UnitListMsg(msg, Some "Attacker")
let defenderMap msg = UnitListMsg(msg, Some "Defender")
let rebindMsg scope map = 
    Option.map (fun (m : Models.Types.Model) -> UnitList.Types.ModelMsg(MathHammer.Models.Types.Msg.Rebind scope, m.Name)
                                                |> map
                                                |> Cmd.ofMsg )
    >> Option.toList
let init () : Model * Cmd<Types.Msg> =
    let (attacker,attackerCmd) = MathHammer.UnitList.State.init "Attacker" () 
    let (defender,defenderCmd) = MathHammer.UnitList.State.init "Defender" () 
    
    let model : Model = 
        
        { 
            Environment = Map.empty<_,_>
            Attacker = { attacker with BoxFill="#FFCCCC"; ElementFill="#79CE0B"; ElementStroke="#396302"; OffsetY = ft.ToMM 2<ft>; Scale="scale(1,-1)" }
            Defender = { defender with BoxFill="#CCCCFF"; ElementFill="#0B79CE"; ElementStroke="#023963" }
            SelectedAttacker = None
            SelectedDefender = None
            Board = 6<ft>,4<ft>
        }
    model, Cmd.batch [ Cmd.map attackerMap attackerCmd
                       Cmd.map defenderMap defenderCmd
                       Cmd.ofMsg RebindEnvironment ]

let update msg model : Model * Cmd<Types.Msg> =
    match msg with
    | UnitListMsg (UnitList.Types.ModelMsg(MathHammer.Models.Types.Msg.Let(Global,name,dist), _), _) -> 
        {model with Environment = Map.add name dist model.Environment}, Cmd.none
    | UnitListMsg (UnitList.Types.ModelMsg(MathHammer.Models.Types.Msg.Select, m), Some "Attacker") -> 
        {model with SelectedAttacker = Some m}, Cmd.ofMsg RebindEnvironment
    | UnitListMsg (UnitList.Types.ModelMsg(MathHammer.Models.Types.Msg.Select, m), Some "Defender") -> 
        {model with SelectedDefender = Some m}, Cmd.ofMsg RebindEnvironment
    | UnitListMsg (msg, Some "Attacker")-> 
        let (ula,ulCmdsa) = UnitList.State.update msg model.Attacker
        printfn "Binding model attacker %A"  msg
        { model with Attacker = ula }, Cmd.batch [ Cmd.map attackerMap ulCmdsa]
    | UnitListMsg (msg, Some "Defender")-> 
        let (uld,ulCmdsd) = UnitList.State.update msg model.Defender
        printfn "Binding model defender %A"  msg
        { model with Defender = uld }, Cmd.batch [ Cmd.map defenderMap ulCmdsd]
    | UnitListMsg (msg, Some _)-> failwith "No list of that name"
    | UnitListMsg (msg, None) -> 
        let (ula,ulCmdsa) = UnitList.State.update msg model.Attacker
        let (uld,ulCmdsd) = UnitList.State.update msg model.Defender
        { model with Attacker = ula; Defender = uld }, Cmd.batch [ Cmd.map attackerMap ulCmdsa
                                                                   Cmd.map defenderMap ulCmdsd ]
    | Swap -> { model with Attacker = { model.Attacker with Models = Map.map (fun k m -> {m with Attributes = List.map(function (name,Let(Defender,str,op)) -> name,Let(Attacker,str,op) | op -> op ) m.Attributes    } ) model.Defender.Models}    
                           Defender = { model.Defender with Models = Map.map (fun k m -> {m with Attributes = List.map(function (name,Let(Attacker,str,op)) -> name,Let(Defender,str,op) | op -> op ) m.Attributes    } ) model.Attacker.Models} 
                           SelectedAttacker = None }, 
                           Cmd.batch [ Cmd.ofMsg ((fun msg -> UnitListMsg(msg, None)) UnitList.Types.Distribute)
                                       Cmd.ofMsg RebindEnvironment ]
    | RebindEnvironment ->      
            {model with Environment = Map.empty<_,_>}, Cmd.batch [ Cmd.ofMsg BindDefender
                                                                   Cmd.ofMsg BindAttacker ]
    | BindDefender -> 
        match model.SelectedDefender with 
        | None -> model, Cmd.none
        | Some defender -> 
            model, (model.SelectedDefender
            |> Option.bind (fun key -> Map.tryFind key model.Defender.Models) 
            |> rebindMsg Defender defenderMap 
            |> Cmd.batch)
    | BindAttacker -> 
        match model.SelectedAttacker with 
        | None -> model, Cmd.none
        | Some attacker -> 
            model, (model.SelectedAttacker
            |> Option.bind (fun key -> Map.tryFind key model.Attacker.Models) 
            |> rebindMsg Attacker attackerMap 
            |> Cmd.batch)
