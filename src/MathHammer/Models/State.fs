module MathHammer.Models.State

open Elmish
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.State

let init name =
    { PosX = 0.
      PosY = 0.
      Name = name
      Size = 28<mm>
      Attributes = Map.empty
      Rules = noValue
      ProbabilityRules = None
      Choices = Map.empty }

let initSgt name coreRules =
    let attributes =
        [ "M", (1, vInt 18)
          "WS", (2, dPlus 6 3)
          "BS", (3, dPlus 6 3)
          "S", (4, vInt 5)
          "T", (5, vInt 4)
          "W", (6, vInt 1)
          "A", (7, vInt 2)
          "Ld", (8, vInt 8)
          "Sv", (9, dPlus 6 3)
          "InvSv", (10, noValue) ]
    
    let (choices, rules) =
        applyArgs coreRules (attributes |> List.map (fun (_, (_, c)) -> c)) 
        |> normalize
    (name, 
     { (init name) with Rules = rules
                        Attributes = Map.ofList attributes }), Cmd.none

let initMeq name coreRules =
    let attributes =
        [ "M", (1, vInt 6)
          "WS", (2, dPlus 6 3)
          "BS", (3, dPlus 6 3)
          "S", (4, vInt 5)
          "T", (5, vInt 4)
          "W", (6, vInt 1)
          "A", (7, vInt 1)
          "Ld", (8, vInt 8)
          "Sv", (9, dPlus 6 3)
          "InvSv", (10, noValue) ]
    
    let (choices, rules) =
        applyArgs coreRules (attributes |> List.map (fun (_, (_, c)) -> c)) 
        |> normalize
    (name, 
     { (init name) with Rules = rules
                        Choices = choices
                        Attributes = Map.ofList attributes }), Cmd.none

let initGeq name coreRules t sv =
    let attributes =
        [ "Weapon Range", (1, vInt 30)
          "M", (2, vInt 6)
          "WS", (3, dPlus 6 3)
          "BS", (4, dPlus 6 3)
          "S", (5, vInt 5)
          "T", (6, vInt t)
          "W", (7, vInt 1)
          "A", (8, vInt 2)
          "Ld", (9, vInt 8)
          "Sv", (10, dPlus 6 sv)
          "InvSv", (11, noValue) ]
    
    let (choices, rules) =
        applyArgs (coreRules |> lam "Weapon Range") 
            (attributes |> List.map (fun (_, (_, c)) -> c)) |> normalize
    (name, 
     { (init name) with Rules = rules
                        Choices = choices
                        Attributes = Map.ofList attributes }), Cmd.none

let update msg model =
    match msg with
    | ChangePosition(x, y) -> 
        { model with PosX = x
                     PosY = y }, Cmd.none
    | Select _ -> model, Cmd.none
    | Rebind(initial) -> 
        let probability = model.Rules |> evalOp initial
        { model with ProbabilityRules = Some probability }, Cmd.none
