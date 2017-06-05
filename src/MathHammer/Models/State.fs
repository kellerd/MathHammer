module MathHammer.Models.State

open Elmish
open Types

let init () =
  {posX=0.;posY=0.;name="";list=""}, Cmd.none

let update msg model =
  match msg with
  | ChangePosition (x,y) -> {model with posX = x; posY = y}, Cmd.none
  | Select _ -> model, Cmd.none
