module GameActions.GameActionsList.State

open Elmish
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.State

let initRow () : Row * Cmd<Types.Msg> = 
    ReadWrite("",  Text None, noValue), Cmd.none

let d6 = App(Call(Dice), Value(Int 6))
let d3 = App(Call(Dice), Value(Int 3))
let opId = Lam ("v",get "v")
let chargeRange = [get "D6";get "D6"] |> opList |> total 
let meleeRange = opList [ get "M"; get "Charge Range" ] |> total |> lam "M"
let shootingRange = get "Weapon Range"
let psychicTest = [get "D6";get "D6"] |> opList |> total 
let denyTest = [get "D6";get "D6"] |> opList |> total
let hitResults = 
    repeatOp (get "WS") (get "A") 
    |> total 
    |> lam "A" 
    |> lam "WS"
let woundResults = 
    repeatOp (svtOps sVsT) (get "Hit Results") 
    |> total 
    |> lam "S" 
    |> lam "Defender"
let unsavedWounds = 
    repeatOp (getd "Sv") (get "Wound Results") 
    |> total 
    |> lam "Defender"
let letTest = 
    bindOp "Some Var" chargeRange (get "D6")   
let twoLetTest =  
    bindOp "Some Var 2" chargeRange (get "Some Var")
    |> bindOp "Some Var" chargeRange   
let nestTest =
    bindOp "Some varNest" letTest twoLetTest    
let globalOperations = 
    [ "Id", Text None, opId
      "D6", Special "D6", d6
      "D3", Special "D3", d3
      "Let Test", Text None, letTest
      "Let Test 2", Text None, twoLetTest
      "Nest Test", Text None, nestTest
      "Charge Range", Text None, chargeRange
      "Assault Range", Text None, meleeRange
      "Shooting Range", Text None, shootingRange
      "Psychic Test", Text None, psychicTest
      "Deny Test", Text None, denyTest
      "To Hit", Text None, hitResults
    //   "To Wound", Text None, woundResults
    //   "Armour Save", Text None, unsavedWounds
    ]    
let init () : Model * Cmd<Types.Msg> =
    let rows = 
        globalOperations
        |> List.map ReadOnly
    { Editing = false 
      Functions = rows 
      Dragging = None }, Cmd.none


let update msg model : Model * Cmd<Types.Msg> =
    match msg with
    | AddRow when not model.Editing -> 
        let (newRow,newRowCmd) = initRow()
        { model with Editing = true; Functions = newRow :: model.Functions }, newRowCmd
    | ChangeNewRowName(str) when model.Editing ->
        let newRows = List.map(function ReadWrite(_,icon, op) -> ReadWrite(str,icon, op) | r -> r) model.Functions
        { model with Functions = newRows } , Cmd.none    
    | ChangeIcon("") when model.Editing ->
        let newRows = List.map(function ReadWrite(str,_, op) -> ReadWrite(str,Text None, op) | r -> r) model.Functions
        { model with Functions = newRows }, Cmd.none 
    | ChangeIcon("D6" | "D3" as s) when model.Editing ->
        let newRows = List.map(function ReadWrite(str,_, op) -> ReadWrite(str,Special s, op) | r -> r) model.Functions
        { model with Functions = newRows }, Cmd.none 
    | ChangeIcon(s) when model.Editing && s.StartsWith("fa-") -> 
        let newRows = List.map(function ReadWrite(str,_, op) -> ReadWrite(str,Icon ("fa " + s), op) | r -> r) model.Functions
        { model with Functions = newRows }, Cmd.none       
    | ChangeIcon(s) when model.Editing ->
        let newRows = List.map(function ReadWrite(str,_, op) -> ReadWrite(str,Text(Some s), op) | r -> r) model.Functions
        { model with Functions = newRows }, Cmd.none        
    | SaveOp (name) when model.Editing -> 
        let newRows = List.map(function ReadWrite(name',icon, op) when name=name' -> ReadOnly(name',icon, op) | r -> r ) model.Functions
        { model with Editing = false; Functions = newRows; Dragging = None }, Cmd.none
    | EditRow (name) when not model.Editing -> 
        let newRows = List.map(function ReadOnly(name',icon, op) when name=name' -> ReadWrite(name',icon, op) | r -> r ) model.Functions
        { model with Editing = true; Functions = newRows; Dragging = None }, Cmd.none 
    | Dragging s when model.Editing ->
        { model with Dragging = Some s }, Cmd.none
    | DragLeft ->
        { model with Dragging = None }, Cmd.none
    | Dragged (name,op) when model.Editing -> 
        match model.Dragging with 
        | None -> model, Cmd.none 
        | Some _ -> 
            let newRows = List.map(function ReadWrite(name', icon, _) when name = name' -> ReadWrite(name, icon, op) | r -> r) model.Functions
            { model with Dragging = None; Functions = newRows }, Cmd.none
    | AddRow _ | SaveOp _ | ChangeNewRowName _ | EditRow _ | ChangeIcon _ | Dragged _ | Dragging _ | ReplaceOp _  ->
        model, Cmd.none