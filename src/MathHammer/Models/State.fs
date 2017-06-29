module MathHammer.Models.State

open Elmish
open Types
open GameActions.Primitives.Types



let hitMelee = 
    Let(Attacker, "MeleeHits", Count[Multiply(Var(Attacker, "WS"), Var(Attacker, "A"))])

let init name =
    { posX=0.
      posY=0.
      name=name
      attributes = []
      size = 28<mm>
      shootingRange = NoValue
      meleeRange = NoValue}

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
                      "Psychic", Ability (Count[Value (Int 5); Value (Int 3); Value (Int 5); NoValue; DPlus (D3,2)])
                      "Balls", Ability (Multiply(Value(Dice(D6)),Value(Dice(D6)))) ] }, Cmd.none
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
