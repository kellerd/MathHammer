module MathHammer.Models.State

open Elmish
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.State
let init name =
    { PosX=0.
      PosY=0.
      Name=name
      Size = 28<mm>
      Attributes = []
      Scale = "scale(1,1)"
      Rules = noValue
      NormalizedRules = noValue
      ProbabilityRules = noValue}

let initSgt name coreRules =
    let attributes = [vInt 6; dPlus D6 3 ;dPlus D6 3; vInt 5; vInt 4; vInt 1; vInt 2; vInt 8; dPlus D6 3; noValue] 
    let rules = applyArgs coreRules attributes
    { (init name) with Rules = rules; Attributes = attributes }, Cmd.none
let initMeq name coreRules =
    let attributes = [vInt 6; dPlus D6 3 ;dPlus D6 3; vInt 4; vInt 4; vInt 1; vInt 1; vInt 8; dPlus D6 3; noValue] 
    let rules = applyArgs coreRules attributes
    { (init name) with Rules = rules; Attributes = attributes }, Cmd.none
let initGeq name coreRules =
    let attributes = [vInt 30; vInt 5; dPlus D6 4 ;dPlus D6 4; vInt 3; vInt 3; vInt 1; vInt 1; vInt 6; dPlus D6 5; noValue] 
    let rules = applyArgs (coreRules |> lam "WeaponRange") attributes
    { (init name) with Rules = rules; Attributes = attributes }, Cmd.none

let (|ContainsVar|_|) env key = Map.tryFind key env
let update msg model =
      match msg with
      | ChangePosition (x,y,scale) -> {model with PosX = x; PosY = y; Scale=scale}, Cmd.none
      | Select _ -> model, Cmd.none
      | Rebind (initial) -> 
            let normalized = model.Rules |> normalize
            let probability = normalized |> evalOp initial
            let cmds = []
            { model with NormalizedRules = normalized
                         ProbabilityRules = probability }, Cmd.batch cmds
