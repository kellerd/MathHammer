module GameActions.GameActionsList.State

open Elmish
open Types
open GameActions.Primitives.Types

let initRow () : Row * Cmd<Types.Msg> = 
    ReadWrite("", Value(NoValue)), Cmd.none

let init () : Model * Cmd<Types.Msg> =
    let readOnlyRow = ReadOnly ("D6", Let(Global, "HighFive", App(Call Total, Value(ManyOp(OpList [Value(Dice(D6)); Value(Int(3))])))))
    ([readOnlyRow],false), Cmd.none


let update msg model : Model * Cmd<Types.Msg> =
    match msg,model with
    | AddRow,(rows,false) -> 
        let (newRow,newRowCmd) = initRow()
        (newRow::rows,true), newRowCmd
    | AddRow,_ -> 
        model, Cmd.none
    | ChangeNewRowName(str),(rs,true) ->
        let newRows = List.map(function ReadWrite(_,op) -> ReadWrite(str,op) | r -> r) rs
        (newRows,true), Cmd.none
    | SaveOp (name),(rs,true) -> 
        let newRows = List.map(function ReadWrite(name',op) when name=name' -> ReadOnly(name',op) | r -> r ) rs
        (newRows,false),Cmd.none
    | EditRow (name),(rs,false) -> 
        let newRows = List.map(function ReadOnly(name',op) when name=name' -> ReadWrite(name',op) | r -> r ) rs
        (newRows,true),Cmd.none 
    | (SaveOp(_) | ChangeNewRowName(_) | EditRow(_)),_->
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
