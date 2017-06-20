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
     attributes = ["WS", Characteristic <| DPlus (D6, 3)
                   "BS", Characteristic <| DPlus (D6, 3)
                   "S" , Characteristic <| Value (Int(4))
                   "T" , Characteristic <| Value (Int(4))
                   "W" , Characteristic <| Value (Int(1))
                   "A" , Characteristic <| Value (Int(2))
                   "LD", Characteristic <| Value (Int(8))
                   "SV", Characteristic <| DPlus (D6, 3)
                   "Psychic", Ability (Sum(Dice(D6),Dice(D6)))
                   "Balls", Ability (Many(Value(Dice(D6)),3)) ] }, Cmd.none
let initGeq name =
  { (init name) with 
     attributes = ["WS", Characteristic <| DPlus (D6, 4)
                   "BS", Characteristic <| DPlus (D6, 4)
                   "S" , Characteristic <| Value (Int(3))
                   "T" , Characteristic <| Value (Int(3))
                   "W" , Characteristic <| Value (Int(1))
                   "A" , Characteristic <| Value (Int(1))
                   "LD", Characteristic <| Value (Int(7))
                   "SV", Characteristic <| DPlus (D6, 5);]}, Cmd.none

let update msg model =
  match msg with
  | ChangePosition (x,y) -> {model with posX = x; posY = y}, Cmd.none
  | Select _ -> model, Cmd.none
