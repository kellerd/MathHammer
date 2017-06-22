module MathHammer.State

open Elmish
open Types

let attackerMap msg = UnitListMsg(msg, Some "Attacker")
let defenderMap msg = UnitListMsg(msg, Some "Defender")

let init () : Model * Cmd<Msg> =
  let (attacker,attackerCmd) = MathHammer.UnitList.State.init "Attacker" () 
  let (defender,defenderCmd) = MathHammer.UnitList.State.init "Defender" () 
  
  let model : Model = 
    
    { 
      Attacker = { attacker with  BoxFill="#FFEEEE"; ElementFill="#79CE0B"; ElementStroke="#396302"; }
      Defender = { defender with  BoxFill="#EEEEFF"; ElementFill="#0B79CE"; ElementStroke="#023963"; OffsetY = 50.}
      Selected = None
      StoredActions = Map.empty<_,_>
    }
  model, Cmd.batch [ Cmd.map attackerMap attackerCmd
                     Cmd.map defenderMap defenderCmd  ]

let update msg model : Model * Cmd<Msg> =
  match msg with
  | UnitListMsg (UnitList.Types.ModelMsg((MathHammer.Models.Types.Msg.Select), m), Some "Attacker") -> 
    let m' = model.Attacker.Models |> Map.find m
    {model with Selected = Some m'}, Cmd.none
  | UnitListMsg (UnitList.Types.ModelMsg((MathHammer.Models.Types.Msg.Select), m), Some "Defender") -> 
    let m' = model.Defender.Models |> Map.find m
    {model with Selected = Some m'}, Cmd.none
  | UnitListMsg (msg, Some "Attacker")-> 
    let (ula,ulCmdsa) = UnitList.State.update msg model.Attacker
    { model with Attacker = ula }, Cmd.batch [ Cmd.map attackerMap ulCmdsa]
  | UnitListMsg (msg, Some "Defender")-> 
    let (uld,ulCmdsd) = UnitList.State.update msg model.Defender
    { model with Defender = uld }, Cmd.batch [ Cmd.map defenderMap ulCmdsd]
  | UnitListMsg (msg, Some _)->  failwith "No list of that name"
  | UnitListMsg (msg, None) -> 
    let (ula,ulCmdsa) = UnitList.State.update msg model.Attacker
    let (uld,ulCmdsd) = UnitList.State.update msg model.Defender
    { model with Attacker = ula; Defender = uld }, Cmd.batch [ Cmd.map attackerMap ulCmdsa
                                                               Cmd.map defenderMap ulCmdsd  ]
  | Swap -> { model with Attacker = { model.Defender with OffsetY = model.Attacker.OffsetY }
                         Defender = { model.Attacker with OffsetY = model.Defender.OffsetY } }, Cmd.ofMsg ((fun msg -> UnitListMsg(msg, None)) UnitList.Types.Distribute)