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
     attributes = ["WS", Characteristic <| DPlus 3;
                   "BS", Characteristic <| DPlus 3;
                   "S" , Characteristic <| Value 4;
                   "T" , Characteristic <| Value 4;
                   "W" , Characteristic <| Value 1;
                   "A" , Characteristic <| Value 2;
                   "LD", Characteristic <| Value 8;
                   "SV", Characteristic <| DPlus 3;
                   "Psychic", Ability (Dice [D6;D6]) ] }, Cmd.none
let initGeq name =
  { (init name) with 
     attributes = ["WS", Characteristic <| DPlus 4;
                   "BS", Characteristic <| DPlus 4;
                   "S" , Characteristic <| Value 3;
                   "T" , Characteristic <| Value 3;
                   "W" , Characteristic <| Value 1;
                   "A" , Characteristic <| Value 1;
                   "LD", Characteristic <| Value 7;
                   "SV", Characteristic <| DPlus 5;]}, Cmd.none

let update msg model =
  match msg with
  | ChangePosition (x,y) -> {model with posX = x; posY = y}, Cmd.none
  | Select _ -> model, Cmd.none
