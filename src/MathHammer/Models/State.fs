module MathHammer.Models.State

open Elmish
open Types
let init name =
  {posX=0.
   posY=0.
   name=name
   attributes = []}
let initMeq name =
  { (init name) with 
     attributes = ["WS", DPlus 3;
                   "BS", DPlus 3;
                   "S" , Value 4;
                   "T" , Value 4;
                   "W" , Value 1;
                   "A" , Value 2;
                   "LD", Value 8;
                   "SV", DPlus 3;]}, Cmd.none
let initGeq name =
  { (init name) with 
     attributes = ["WS", DPlus 4;
                   "BS", DPlus 4;
                   "S" , Value 3;
                   "T" , Value 3;
                   "W" , Value 1;
                   "A" , Value 1;
                   "LD", Value 7;
                   "SV", DPlus 5;]}, Cmd.none

let update msg model =
  match msg with
  | ChangePosition (x,y) -> {model with posX = x; posY = y}, Cmd.none
  | Select _ -> model, Cmd.none
