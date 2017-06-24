module GameActions.GameActionsList.State

open Elmish
open Types
open GameActions.Primitives.Types

let init () : Model * Cmd<Msg> =
  ["D6", Total[Value(Dice(D6)); Value(Int(3))]
  ], Cmd.none


let update msg model : Model * Cmd<Msg> =
  model, Cmd.none
  // match msg with
  // | Distribute -> 
  //     let (newModels, modelsCmds) =
  //       model.Models
  //       |> Map.toList
  //       |> distribute 0. model.OffsetY 
  //       |> List.map(fun((_,m),x,y) -> MathHammer.Models.State.update (MathHammer.Models.Types.Msg.ChangePosition(x,y)) m)
  //       |> List.fold(fun (map,cmds) (m,cmd) -> (Map.add m.name m map), cmd::cmds) (model.Models,[])
  //     {model with Models = newModels}, Cmd.batch (modelsCmds)
  // | ModelMsg(msg,key) -> 
  //     let (newModel, modelCmds) = model.Models.Item(key) |> MathHammer.Models.State.update msg
  //     {model with Models = Map.add key newModel model.Models}, Cmd.map ModelMsg modelCmds
