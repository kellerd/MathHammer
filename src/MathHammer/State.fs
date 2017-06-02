module MathHammer.State

open Elmish
open Types

let init () : Model * Cmd<Msg> =
  let (unitList,unitListCmd) = MathHammer.UnitList.State.init()
  
  let model : Model = 
    
    { 
      Attacker = { unitList with  BoxFill="#FFEEEE"; ElementFill="#79CE0B"; ElementStroke="#396302"}
      Defender = { unitList with  BoxFill="#EEEEFF"; ElementFill="#0B79CE"; ElementStroke="#023963"; OffsetY = 50.}
    }
  model, Cmd.batch [ Cmd.map UnitListMsg unitListCmd ]

let update msg model : Model * Cmd<Msg> =
  match msg with
  | UnitListMsg msg-> 
    let (ula,ulCmdsa) = UnitList.State.update msg model.Attacker
    let (uld,ulCmdsd) = UnitList.State.update msg model.Defender
    { model with Attacker = ula; Defender = uld }, Cmd.batch [ Cmd.map UnitListMsg ulCmdsa
                                                               Cmd.map UnitListMsg ulCmdsd  ]
  | Swap -> { model with Attacker = { model.Defender with OffsetY = model.Attacker.OffsetY };
                         Defender = { model.Attacker with OffsetY = model.Defender.OffsetY } }, Cmd.ofMsg (UnitListMsg UnitList.Types.Distribute)
 