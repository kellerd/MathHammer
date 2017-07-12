module MathHammer.Models.State

open Elmish
open Types
open GameActions.Primitives.Types


let wsTest = Count(OpList[Var(Attacker, "WS")])
let hitMelee = 
    Let(Attacker, "Melee", Let(Attacker, "MeleeHits", Total(Unfold(wsTest, Var(Attacker, "A")))))
let shotsMelee = 
    Let(Attacker, "Shots", Let(Attacker, "Shots", Count <| OpList [Product <| OpList [Var(Attacker, "A"); Var(Attacker, "A")]]))

let init name =
    { PosX=0.
      PosY=0.
      Name=name
      Attributes = []
      Size = 28<mm>
      Scale = "scale(1,1)"
      ShootingRange = NoValue
      MeleeRange = NoValue}

let initMeq name env =
    { (init name) with 
        //ShootingRange = Value(Int(24)) 
        MeleeRange = Total <| OpList [Value(Int(7));Value(Dice(D6));Value(Dice(D6));Value(Dice(D6))]
        Attributes = ["M",  Let(env, "M",  Value(Int(6)))
                      "WS", Let(env, "WS", DPlus (D6, 3))
                      "BS", Let(env, "BS", DPlus (D6, 3))
                      "S" , Let(env, "S" , Value (Int(4)))
                      "T" , Let(env, "T" , Value (Int(4)))
                      "W" , Let(env, "W" , Value (Int(1)))
                      "A" , Let(env, "A" , Value (Int(2)))
                      "LD", Let(env, "LD", Value (Int(8)))
                      "SV", Let(env, "SV", DPlus (D6, 3))
                      "Psychic", Let(env, "Psychic", Let(env, "PsychicResult", Total <| OpList [Value(Dice(D6));Value(Dice(D6))])) 
                      "Melee", hitMelee
                      "Shots",shotsMelee ] }, Cmd.none
let initGeq name env =
    { (init name) with 
        ShootingRange = Total <| OpList [Value(Int(6))]
        Attributes = ["M",  Let(env, "M",  Value(Int(5)))
                      "WS", Let(env, "WS", DPlus (D6, 4))
                      "BS", Let(env, "BS", DPlus (D6, 4))
                      "S" , Let(env, "S" , Value (Int(3)))
                      "T" , Let(env, "T" , Value (Int(3)))
                      "W" , Let(env, "W" , Value (Int(1)))
                      "A" , Let(env, "A" , Value (Int(1)))
                      "LD", Let(env, "LD", Value (Int(7)))
                      "SV", Let(env, "SV", DPlus (D6, 5))]}, Cmd.none

let update msg model =
    match msg with
    | ChangePosition (x,y,scale) -> {model with PosX = x; PosY = y; Scale=scale}, Cmd.none
    | Select _ -> model, Cmd.none
    | Msg.Let _ ->  model, Cmd.none
