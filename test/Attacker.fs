module AttackerTests
open GameActions.Primitives.Types
open GameActions.Primitives.State
open Expecto
open App.State
open GameActions.GameActionsList.State
let (==?) x y = Expect.equal x y ""
let (==~) x y = 
    match y with 
    | Value(Dist({Probabilities = y})) -> Expect.contains (y |> List.map (fst >> Value)) x ""
    | _ -> Expect.equal x y ""

let phaseActions = 
    choose "Phase" 
        [
            "Melee", 
                nestOps 
                    [ get "ChargeRange"     >>= "ChargeRange"
                      get "MeleeRange"      <*> get "M" 
                                            >>= "MeleeRange"
                      get "ToHit"           <*> get "WS" 
                                            <*> get "A" 
                                            >>= "HitResults"
                                        
                      get "ToWound"         <*> get "Defender" 
                                            <*> get "S" 
                                            >>= "WoundResults"
                      get "ArmourSave"      <*> get "Defender"  
                                            >>= "UnsavedWounds" ] 
                                                          <| opList [ labelVar "ChargeRange"
                                                                      labelVar "MeleeRange"
                                                                      labelVar "HitResults"
                                                                      labelVar "WoundResults"
                                                                      labelVar "UnsavedWounds" ] 
            "Shooting", (get "ShootingRange" >>= "ShootingRange") (labelVar "ShootingRange")
            "Psychic", (get "PsychicTest" >>= "Psychichtest") (labelVar "PsychicTest")
        ] >>= "Actions"    

let allProps = 
    opList 
        [ labelVar "M"
          labelVar "WS"
          labelVar "BS"
          labelVar "S"
          labelVar "T"
          labelVar "W"
          labelVar "A"
          labelVar "Ld"
          labelVar "Sv"
          labelVar "InvSv"
          labelVar "D6Test"
          labelVar "D3Test"
          labelProp "Actions" "ChargeRange"
          labelProp "Actions" "MeleeRange"
          labelProp "Actions" "HitResults"
          labelProp "Actions" "WoundResults"
          labelProp "Actions" "UnsavedWounds"
          labelProp "Actions" "PsychicTest"
          labelProp "Actions" "ShootingRange"
          labelProp "Actions" "DenyTest" 
          ]
let body = nestOps [phaseActions] allProps
let defbody = nestOps [dPhaseActions] allProps
let stats = ["M";"WS";"BS";"S";"T";"W";"A";"Ld";"Sv";"InvSv"]  
let attacker = createArgs stats body
let defender =  createArgs stats defbody
let move = vInt 6
let threePlus = dPlus D6 3
let ws = threePlus
let bs = threePlus
let s = vInt 4
let t = vInt 4
let w = vInt 1
let a = vInt 2
let ld = vInt 8
let sv = threePlus
let invSave = noValue
let seargent = [move;ws;bs;s;t;w;a;ld;sv;invSave;] 
let globalOperations = 
    [ "Id", opId
      "D6",  d6
      "D3", d3
      "ChargeRange",  chargeRange
      "MeleeRange",  meleeRange
      "ShootingRange",  shootingRange
      "PsychicTest",  psychicTest
      "DenyTest",  denyTest
      "ToHit",  hitResults
      "ToWound",  woundResults
      "ArmourSave",  unsavedWounds ]
    |> List.mapi(fun i (k,o) -> (k,(i,o)))
let choices = 
    ["Phase",  Set.singleton  "Melee"] |> Map.ofList
let env = 
    ["Phase",  Value (Str  "Melee")] |> Map.ofList
let (newChoices,newEnv) = 
    globalOperations
    |> List.sortBy (snd >> fst)
    |> List.fold(fun (choices,env) (key,(_,op))-> 
            let (newChoices,normal) = op |> normalize 
            let result = normal |> evalOp env
            Map.mergeSets choices newChoices, Map.add key result env) (choices, env)    
let defApplied = applyArgs defender seargent |> normalize |> snd |> evalOp newEnv
let initialMap = Map.add "Defender" defApplied newEnv
let attApplied = applyArgs attacker seargent |> normalize |> snd
let eval x op = getp x op |> evalOp initialMap 

    // App
    //    (App
    //       (  Lam
    //             ("WS",
    //              Lam
    //                ("A",
    //                 App
    //                   (Call Total,
    //                    App
    //                      (Call Repeat,
    //                       Value (ParamArray [Lam ("unusedVariable",Var "WS"); Var "A"]))))),
    //        App
    //          (Call Count,
    //           Let
    //             ("roll",App (Call (Dice D6),Value NoValue),
    //              Let
    //                ("gt",
    //                 App
    //                   (Call GreaterThan,
    //                    Value (ParamArray [Var "roll"; Value (Int 3)])),
    //                 Let
    //                   ("eq",
    //                    App
    //                      (Call Equals,
    //                       Value (ParamArray [Var "roll"; Value (Int 3)])),
    //                    App (Call Or,Value (ParamArray [Var "eq"; Var "gt"]))))))),
    //     Value (Int 2))
    // |> (evalOp  initialMap )

// getp "ChargeRange" attApplied  |> (evalOp  initialMap )
// getp "MeleeRange" attApplied  |> (evalOp  initialMap )
// getp "HitResults" attApplied  |> (evalOp initialMap )
// getp "WoundResults" attApplied  |> (evalOp initialMap )

// attApplied  |> (evalOp  <| Map.add "Phase" (Value(Str "Psychic")) initialMap )
// let x = repeatOp (vInt 4) (vInt 4) |> evalOp Map.empty<_,_>  
// let a' = vInt 2
// a' |> count |> evalOp Map.empty<_,_> 
// let ws' = dPlus D6 3
// ws' |> evalOp Map.empty<_,_> 
// ws' |> count |> evalOp Map.empty<_,_> 
// let hits' = repeatOp (ws' |> count ) (a') |> total |> evalOp Map.empty<_,_>  
// let svst' = dPlus D6 3 
// svst' |> evalOp Map.empty<_,_> 
// svst' |> count |> evalOp Map.empty<_,_> 
// let wounds' = repeatOp (svst' |> count   ) (hits') |> evalOp Map.empty<_,_>
// let wounds'' = repeatOp (svst' |> count ) (hits') |> total |> evalOp Map.empty<_,_>  
// let x = d6 |> count  |> evalOp Map.empty<_,_>  
// let D6D6s = repeatOp (d6) (d6)  |> total  |> evalOp Map.empty<_,_> 
// let TwoD6 = repeatOp (d6) (vInt 2) |> total  |> evalOp Map.empty<_,_>  
// let x = repeatOp (vInt 4) (d6)  |> evalOp Map.empty<_,_> 
// let x = repeatOp (d6) (d6)  |> evalOp Map.empty<_,_>  
// let x = repeatOp (vInt 4) (Value(ParamArray([vInt 3; vInt 2])))  |> evalOp Map.empty<_,_> 
// let x = repeatOp (Value(ParamArray([vInt 3; vInt 2]))) (vInt 4)  |> evalOp Map.empty<_,_> 
//|> count |> evalOp initialMap

[<Tests>]
let tests = 
    let pairs = List.zip stats seargent 
    let expectedStd = 
        let expectedDice n plus = 
            [1..n] 
            |> List.map (fun i -> 
                            if i >= plus then 
                                Check(Check.Pass (Int 1)),1.0 
                            else 
                                Check(Check.Fail (Int 1)),1.0) 
            |> Distribution.countedCases
            |> Dist
            |> Value
        pairs        
        |> List.map(function 
                    | key,IsDPlus (D6,plus) -> key, expectedDice 6 plus
                    | key,IsDPlus (D3,plus) -> key, expectedDice 3 plus
                    | key,op -> key,op)
    testList "Attacker Tests" [
        expectedStd 
        |> List.map(fun (key,expected) -> test (sprintf "Check %s" key) { eval key attApplied ==? expected })  
        |> testList "Std eval Tests";
    ] 
// let attackerLam = <@ 
//     let m = 6
//     let a = 5
//     ["M",m; "A",a] @>
// let meq = 
//     emptyOp
//     |> chargeRange
//     |> (vInt 5 |~> "M") 

// let test char = get char |> single |> count
// let hitMelee = 
//     get Attacker "A"
//     |> repeatOp (test "WS")
//     |> total 
//     |> bindOp Attacker "Melee"

// let shotsMelee = 
//     [Var(Attacker, "A"); Var(Attacker, "A")]
//     |> opList 
//     |> product
//     |> single
//     |> count
//     |> bindOp Attacker "Shots"

// let dPlus d v = Value(DPlus(d,v))
// let vInt i = Value(Int(i))
// let d6 = Value(Dice(D6))

// let chargeRange = [d6;d6] |> opList |> total |> bindOp Attacker "ChargeRange"
// let meleeRange = [get Attacker "M"; chargeRange] |> opList |> total |> bindOp Attacker "MeleeRange"
// let psychicTest = [d6;d6] |> opList |> total |> bindOp Attacker "PsychicTest"

// let sUser = get Attacker "S"
// let r = 
//  <@
//     let claws (a:int) (b:int) (c:int) = 
//         let x = 5
//         let y = 6
//         x + 7 + y
//     claws 5 3 1 @>
//   Let (claws,
//      Lambda (a,
//              Lambda (b,
//                      Lambda (c,
//                              Let (x, Value (5),
//                                   Let (y, Value (6),
//                                        Call (None, op_Addition,
//                                              [Call (None, op_Addition,
//                                                     [x, Value (7)]), y])))))),
//      Application (Application (Application (claws, Value (5)), Value (3))