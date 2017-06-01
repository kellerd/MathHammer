module MathHammer.Model.State

open Elmish
open Types

let init () : Model * Cmd<Msg> =
  {posX=0.;posY=0.;name=""}, []

let update msg model : Model * Cmd<Msg> =
  match msg with
  | () -> model, []
