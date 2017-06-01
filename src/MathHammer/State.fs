module MathHammer.State

open Elmish
open Types

let init () : Model * Cmd<Msg> =
  let (unitList,unitListCmd) = MathHammer.UnitList.State.init()
  let model = 
    { 
      Attacker = { MathHammer.UnitList.State.init() with Models = ["Marine";"Captain"]} ()
      Defender = { MathHammer.UnitList.State.init() with Models = (['a'..'z'] |> List.map(fun c ->c.ToString())) }
    }, []
  model, Cmd.batch [ cmd
                  Cmd.map CounterMsg counterCmd
                  Cmd.map HomeMsg homeCmd
                  Cmd.map MathHammerMsg mathHammerCmd ]

let update msg model : Model * Cmd<Msg> =
  match msg with
  | () -> model, []
