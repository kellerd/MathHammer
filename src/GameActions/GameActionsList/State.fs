module GameActions.GameActionsList.State

open Elmish
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.State

let initRow() : Row * Cmd<Types.Msg> =
    ReadWrite("", Text None, noValue), Cmd.none
let d6 = App(Call(Dice), Value(Int 6))
let d3 = App(Call(Dice), Value(Int 3))
let opId = Lam("v", get "v")

let chargeRange =
    [ get "D6"
      get "D6" ]
    |> opList
    |> total

let meleeRange =
    opList [ get "M"
             get "Charge Range" ]
    |> total
    |> lam "Charge Range"
    |> lam "M"

let shootingRange = get "Weapon Range"

let psychicTest =
    [ get "D6"
      get "D6" ]
    |> opList
    |> total

let denyTest =
    [ get "D6"
      get "D6" ]
    |> opList
    |> total

let rangeCheck =
    let greater =
        [ get "Assault Range"
          get "Range" ]
        |> opList
        |> call GreaterThan
    
    let eq =
        [ get "Assault Range"
          get "Range" ]
        |> opList
        |> call Equals
    
    [ greater; eq ]
    |> opList
    |> call Or
    |> count
    |> lam "Assault Range"
    |> lam "Range"

let toHit =
    repeatOp (get "Can Hit") (repeatOp (get "WS") (get "A") |> total)
    |> total
    |> lam "Can Hit"
    |> lam "A"
    |> lam "WS"

let table ifThen =
    let rec acc x =
        match x with
        | [] -> None
        | (cmp, thenPortion) :: xs -> 
            IfThenElse(cmp, thenPortion, acc xs) |> Some
    match acc ifThen with
    | None -> noValue
    | Some o -> o

let sVsT =
    [ [ get "S"
        getd "T" ]
      |> opList
      |> call GreaterThan, dPlus 6 3
      [ get "S"
        getd "T" ]
      |> opList
      |> call LessThan, dPlus 6 5
      [ get "S"
        getd "T" ]
      |> opList
      |> call Equals, dPlus 6 4 ]
    |> table
    |> lam "S"
    |> lam "Defender"

let toWound =
    repeatOp (get "Strength vs Toughness Table") (get "Hit Results")
    |> total
    |> lam "Strength vs Toughness Table"
    |> lam "Hit Results"

let armourSave =
    repeatOp (getd "Sv") (get "Wound Results")
    |> total
    |> lam "Wound Results"
    |> lam "Defender"

let allinOne =
    bindOp "Hit Results" (repeatOp (get "WS") (get "A") |> total) 
        (bindOp "Wound Results" (repeatOp ([ [ get "S"
                                               getd "T" ]
                                             |> opList
                                             |> call GreaterThan, dPlus 6 3
                                             [ get "S"
                                               getd "T" ]
                                             |> opList
                                             |> call LessThan, dPlus 6 5
                                             [ get "S"
                                               getd "T" ]
                                             |> opList
                                             |> call Equals, dPlus 6 4 ]
                                           |> table) (get "Hit Results")
                                 |> total) (get "Wound Results"))
    |> lam "Defender"

let letTest = bindOp "Some Var" chargeRange (get "D6")
let twoLetTest =
    bindOp "Some Var 2" chargeRange (get "Some Var") 
    |> bindOp "Some Var" chargeRange
let nestTest = bindOp "Some varNest" letTest twoLetTest
let lam1Test = Lam("x", Value(Int 6))
let lam2Test = Lam("x", Var "x")

let appTest =
    App
        (App(Lam("x", 
                 Lam("y", 
                     Lam("z", 
                         Value(ParamArray [ Var "x"
                                            Var "y"
                                            Value(Int 4) ])))), Value(Int 2)), 
         Value(Int 3))

let app2Test =
    App(App(Lam("x", 
                Lam("y", 
                    Lam("z", 
                        [ psychicTest
                          get "y" ]
                        |> opList
                        |> total))), Value(Int 2)), psychicTest)

let globalOperations =
    [ "Id", Text None, opId
      "D6", Special "D6", d6
      "D3", Special "D3", d3
      "App 2 Test", Text None, app2Test
      "App Test", Text None, appTest
      "Lam Test", Text None, lam1Test
      "Nest Test", Text None, nestTest
      "Let Test", Text None, letTest
      "Let Test 2", Text None, twoLetTest
      "Lam 2 Test", Text None, lam2Test
      "Strength vs Toughness Table", Text None, sVsT
      "Charge Range", Text None, chargeRange
      "Assault Range", Text None, meleeRange
      "Range Check", Text None, rangeCheck
      "Shooting Range", Text None, shootingRange
      "Psychic Test", Text None, psychicTest
      "Deny Test", Text None, denyTest
      "To Hit", Text None, toHit
      "To Wound", Text None, toWound
      "Armour Save", Text None, armourSave
      "All in one", Text None, allinOne ]

let init() : Model * Cmd<Types.Msg> =
    let rows =
        globalOperations |> List.map (fun (a, b, operation) -> 
                                let (choices, normalized) = normalize operation
                                choices, ReadOnly(a, b, operation, normalized))
    { Editing = false
      Functions = rows
      Dragging = None }, Cmd.none

let update msg model : Model * Cmd<Types.Msg> =
    match msg with
    | AddRow when not model.Editing -> 
        let (newRow, newRowCmd) = initRow()
        { model with Editing = true
                     Functions = (Map.empty<_, _>, newRow) :: model.Functions }, 
        newRowCmd
    | ChangeNewRowName(str) when model.Editing -> 
        let newRows =
            List.map (function 
                | choices, ReadWrite(_, icon, op) -> 
                    choices, ReadWrite(str, icon, op)
                | r -> r) model.Functions
        { model with Functions = newRows }, Cmd.none
    | ChangeIcon("") when model.Editing -> 
        let newRows =
            List.map (function 
                | choices, ReadWrite(str, _, op) -> 
                    choices, ReadWrite(str, Text None, op)
                | r -> r) model.Functions
        { model with Functions = newRows }, Cmd.none
    | ChangeIcon("D6" | "D3" as s) when model.Editing -> 
        let newRows =
            List.map (function 
                | choices, ReadWrite(str, _, op) -> 
                    choices, ReadWrite(str, Special s, op)
                | r -> r) model.Functions
        { model with Functions = newRows }, Cmd.none
    | ChangeIcon(s) when model.Editing && s.StartsWith("fa-") -> 
        let newRows =
            List.map (function 
                | choices, ReadWrite(str, _, op) -> 
                    choices, ReadWrite(str, Icon("fa " + s), op)
                | r -> r) model.Functions
        { model with Functions = newRows }, Cmd.none
    | ChangeIcon(s) when model.Editing -> 
        let newRows =
            List.map (function 
                | choices, ReadWrite(str, _, op) -> 
                    choices, ReadWrite(str, Text(Some s), op)
                | r -> r) model.Functions
        { model with Functions = newRows }, Cmd.none
    | SaveOp(name) when model.Editing -> 
        let newRows =
            List.map (function 
                | _, ReadWrite(name', icon, op) when name = name' -> 
                    let (choices, normalized) = normalize op
                    choices, ReadOnly(name', icon, op, normalized)
                | r -> r) model.Functions
        { model with Editing = false
                     Functions = newRows
                     Dragging = None }, Cmd.none
    | EditRow(name) when not model.Editing -> 
        let newRows =
            List.map (function 
                | choices, ReadOnly(name', icon, op, _) when name = name' -> 
                    choices, ReadWrite(name', icon, op)
                | r -> r) model.Functions
        { model with Editing = true
                     Functions = newRows
                     Dragging = None }, Cmd.none
    | Dragging s when model.Editing -> 
        { model with Dragging = Some s }, Cmd.none
    | DragLeft -> { model with Dragging = None }, Cmd.none
    | Dragged(name, op) when model.Editing -> 
        match model.Dragging with
        | None -> model, Cmd.none
        | Some _ -> 
            let newRows =
                List.map (function 
                    | choices, ReadWrite(name', icon, _) when name = name' -> 
                        choices, ReadWrite(name, icon, op)
                    | r -> r) model.Functions
            { model with Dragging = None
                         Functions = newRows }, Cmd.none
    | AddRow _ | SaveOp _ | ChangeNewRowName _ | EditRow _ | ChangeIcon _ | Dragged _ | Dragging _ | ReplaceOp _ -> 
        model, Cmd.none
