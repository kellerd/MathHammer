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
      Attributes = Map.empty<_,_>
      Scale = "scale(1,1)"
      Rules = noValue
      NormalizedRules = noValue
      ProbabilityRules = noValue }

let initSgt name coreRules =
    let attributes = [ "M",     ( 1, vInt 6)
                       "WS",    ( 2, dPlus D6 3)
                       "BS",    ( 3, dPlus D6 3)
                       "S",     ( 4, vInt 5)
                       "T",     ( 5, vInt 4)
                       "W",     ( 6, vInt 1)
                       "A",     ( 7, vInt 2)
                       "Ld",    ( 8, vInt 8)
                       "Sv",    ( 9, dPlus D6 3)
                       "InvSv", (10, noValue)] 
    let rules = applyArgs coreRules (attributes |> List.map (fun (_,(_,c)) -> c))
    { (init name) with Rules = rules; Attributes = Map.ofList attributes }, Cmd.none
let initMeq name coreRules =
    let attributes = [ "M",     ( 1, vInt 6)
                       "WS",    ( 2, dPlus D6 3)
                       "BS",    ( 3, dPlus D6 3)
                       "S",     ( 4, vInt 5)
                       "T",     ( 5, vInt 4)
                       "W",     ( 6, vInt 1)
                       "A",     ( 7, vInt 1)
                       "Ld",    ( 8, vInt 8)
                       "Sv",    ( 9, dPlus D6 3)
                       "InvSv", (10, noValue   )] 
    let rules = applyArgs coreRules (attributes |> List.map (fun (_,(_,c)) -> c))
    { (init name) with Rules = rules; Attributes =  Map.ofList attributes }, Cmd.none
let initGeq name coreRules =
    let attributes = [ "WeaponRange",( 1, vInt 30)
                       "M",          ( 2, vInt 6)
                       "WS",         ( 3, dPlus D6 3)
                       "BS",         ( 4, dPlus D6 3)
                       "S",          ( 5, vInt 5)
                       "T",          ( 6, vInt 4)
                       "W",          ( 7, vInt 1)
                       "A",          ( 8, vInt 2)
                       "Ld",         ( 9, vInt 8)
                       "Sv",         (10, dPlus D6 3)
                       "InvSv",      (11,  noValue  )]  
    let rules = applyArgs (coreRules |> lam "WeaponRange") (attributes |> List.map (fun (_,(_,c)) -> c))
    { (init name) with Rules = rules; Attributes = Map.ofList attributes }, Cmd.none

let update msg model =
      match msg with
      | ChangePosition (x,y,scale) -> {model with PosX = x; PosY = y; Scale=scale}, Cmd.none
      | Select _ -> model, Cmd.none
      | MakeChoice _ -> model, Cmd.none
      | Rebind (initial) -> 
            let (choices,normalized) = model.Rules |> normalize
            let probability = normalized |> evalOp initial
            let cmds = choices |> Map.toList |> List.map (MakeChoice >> Cmd.ofMsg)
            { model with NormalizedRules = normalized
                         ProbabilityRules = probability }, Cmd.batch cmds
