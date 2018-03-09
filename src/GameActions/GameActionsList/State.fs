module GameActions.GameActionsList.State

open Elmish
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.State

let initRow () : Row * Cmd<Types.Msg> = 
    ReadWrite("",  Text None, noValue), Cmd.none

let d6 = App(Call(Dice(D6)), noValue)
let d3 = App(Call(Dice(D3)), noValue)
let opId = Lam ("v",get "v")
let chargeRange = [d6;d6] |> opList |> total 
let meleeRange = opList [ get "M"; get "ChargeRange" ] |> total 
let shootingRange = get "WeaponRange"
let psychicTest = [d6;d6] |> opList |> total 
let denyTest = [d6;d6] |> opList |> total
let hitResults = repeatOp (get "WS") (get "A") |> total
let woundResults = repeatOp (svtOps sVsT) (get "HitResults") |> total 
let unsavedWounds = repeatOp (getd "Sv") (get "WoundResults") |> total 
let init () : Model * Cmd<Types.Msg> =
    let rows = 
        [ "Id", Text None, opId
          "D6", Special "D6", d6
          "D3", Special "D3", d3
          "ChargeRange", Text None, chargeRange
          "MeleeRange", Text None, meleeRange
          "ShootingRange", Text None, shootingRange
          "PsychicTest", Text None, psychicTest
          "DenyTest", Text None, denyTest
          "HitResults", Text None, hitResults
          "WoundResults", Text None, woundResults
          "UnsavedWounds", Text None, unsavedWounds ]
        |> List.map ReadOnly
    (rows,false), Cmd.none


let update msg model : Model * Cmd<Types.Msg> =
    match msg,model with
    | AddRow,(rows,false) -> 
        let (newRow,newRowCmd) = initRow()
        (newRow::rows,true), newRowCmd
    | AddRow,_ -> 
        model, Cmd.none
    | ChangeNewRowName(str),(rs,true) ->
        let newRows = List.map(function ReadWrite(_,icon, op) -> ReadWrite(str,icon, op) | r -> r) rs
        (newRows,true), Cmd.none    
    | ChangeIcon(""),(rs,true) ->
        let newRows = List.map(function ReadWrite(str,_, op) -> ReadWrite(str,Text None, op) | r -> r) rs
        (newRows,true), Cmd.none 
    | ChangeIcon("D6" | "D3" as s),(rs,true) ->
        let newRows = List.map(function ReadWrite(str,_, op) -> ReadWrite(str,Special s, op) | r -> r) rs
        (newRows,true), Cmd.none 
    | ChangeIcon(s),(rs,true) when s.StartsWith("fa-") -> 
        let newRows = List.map(function ReadWrite(str,_, op) -> ReadWrite(str,Icon ("fa " + s), op) | r -> r) rs
        (newRows,true), Cmd.none       
    | ChangeIcon(s),(rs,true) ->
        let newRows = List.map(function ReadWrite(str,_, op) -> ReadWrite(str,Text(Some s), op) | r -> r) rs
        (newRows,true), Cmd.none        
    | SaveOp (name),(rs,true) -> 
        let newRows = List.map(function ReadWrite(name',icon, op) when name=name' -> ReadOnly(name',icon, op) | r -> r ) rs
        (newRows,false),Cmd.none
    | EditRow (name),(rs,false) -> 
        let newRows = List.map(function ReadOnly(name',icon, op) when name=name' -> ReadWrite(name',icon, op) | r -> r ) rs
        (newRows,true),Cmd.none 
    | (SaveOp(_) | ChangeNewRowName(_) | EditRow(_)),_ | ChangeIcon(_),_ ->
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
