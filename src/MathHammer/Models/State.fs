module MathHammer.Models.State

open Elmish
open Types
open GameActions.Primitives.Types



let hitMelee = 
    Let(Attacker, "MeleeHits", Count[Multiply(Var(Attacker, "WS"), Var(Attacker, "A"))])

let init name =
    { PosX=0.
      PosY=0.
      Name=name
      Attributes = []
      Size = 28<mm>
      Scale = "scale(1,1)"
      ShootingRange = NoValue
      MeleeRange = NoValue}

let initMeq name =
    { (init name) with 
        //ShootingRange = Value(Int(24)) 
        MeleeRange = Total[Value(Int(7));Value(Dice(D6));Value(Dice(D6));Value(Dice(D6))]
        Attributes = ["M",  Characteristic <| Value(Int(6))
                      "WS", Characteristic <| DPlus (D6, 3)
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
        Attributes = ["M",  Characteristic <| Value(Int(5))
                      "WS", Characteristic <| DPlus (D6, 4)
                      "BS", Characteristic <| DPlus (D6, 4)
                      "S" , Characteristic <| Value (Int(3))
                      "T" , Characteristic <| Value (Int(3))
                      "W" , Characteristic <| Value (Int(1))
                      "A" , Characteristic <| Value (Int(1))
                      "LD", Characteristic <| Value (Int(7))
                      "SV", Characteristic <| DPlus (D6, 5);]}, Cmd.none

let update msg model =
    match msg with
    | ChangePosition (x,y,scale) -> {model with PosX = x; PosY = y; Scale=scale}, Cmd.none
    | Select _ -> model, Cmd.none
