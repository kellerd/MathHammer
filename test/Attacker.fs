module AttackerTests

open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.State
open GameActions.GameActionsList.State
open Expecto

let (==?) x y = 
    match x,y with
    | Value(Dist({ Probabilities = x })), Value(Dist({ Probabilities = y })) -> 
        let lists = 
            List.zip (List.sort x) (List.sort y) 
        Expect.all lists (fun ((a,pa), (b,pb)) -> Expect.floatClose Accuracy.medium pa pb ""; a = b )  "" 
    | _ -> Expect.equal x y ""

let (==~) x y =
    match y with
    | Value(Dist({ Probabilities = y })) -> Expect.contains (y |> List.map (fst >> Value)) x ""
    | _ -> Expect.equal x y ""

// let phaseActions = 
//     choose "Phase" 
//         [
//             "Assault", 
//                 nestOps 
//                     [ get "Assault Range"   <*> get "M" 
//                                             >>= "Assault Range"
//                       get "To Hit"           <*> get "WS" 
//                                             <*> get "A" 
//                                             >>= "Hit Results"
//                       get "To Wound"         <*> get "Defender" 
//                                             <*> get "S" 
//                                             >>= "Wound Results"
//                       get "Armour Save"      <*> get "Defender"  
//                                             >>= "Unsaved Wounds" ] 
//                                                           <| opList [ labelVar "Charge Range"
//                                                                       labelVar "Assault Range"
//                                                                       labelVar "Hit Results"
//                                                                       labelVar "Wound Results"
//                                                                       labelVar "Unsaved Wounds" ] 
//             "Shooting", labelVar "Shooting Range"
//             "Psychic", labelVar "Psychic Test"
//         ] >>= "Actions"    
// let allProps = 
//     opList 
//         [ labelVar "M"
//           labelVar "WS"
//           labelVar "BS"
//           labelVar "S"
//           labelVar "T"
//           labelVar "W"
//           labelVar "A"
//           labelVar "Ld"
//           labelVar "Sv"
//           labelVar "InvSv"
//           labelVar "D6Test"
//           labelVar "D3Test"
//           labelProp "Actions" "Charge Range"
//           labelProp "Actions" "Assault Range"
//           labelProp "Actions" "Hit Results"
//           labelProp "Actions" "Wound Results"
//           labelProp "Actions" "Unsaved Wounds"
//           labelProp "Actions" "Psychic Test"
//           labelProp "Actions" "Shooting Range"
//           labelProp "Actions" "Deny Test" 
//           ]
let body = nestOps [ phaseActions ] allPropsa
let defbody = nestOps [ dPhaseActions ] allPropsd
let stats = [ "M"; "WS"; "BS"; "S"; "T"; "W"; "A"; "Ld"; "Sv"; "InvSv" ]
let attacker = createArgs stats body
let defender = createArgs stats defbody
let move = vInt 6
let threePlus = dPlus 6 3
let ws = threePlus
let bs = threePlus
let s = vInt 4
let t = vInt 4
let w = vInt 1
let a = vInt 2
let ld = vInt 8
let sv = threePlus
let invSave = noValue
let seargent = [ move; ws; bs; s; t; w; a; ld; sv; invSave ]
let choices = Map.empty<_, _>
let env = Map.empty<_, _>

let (newChoices, newEnv) =
    globalOperations
    |> List.mapi (fun i (k, _, o) -> (k, (i, o)))
    |> List.sortBy (snd >> fst)
    |> List.fold (fun (choices, env) (key, (_, op)) -> 
           let (newChoices, normal) = op |> normalize
           let result = normal |> evalOp env
           Map.mergeSets choices newChoices, Map.add key result env) (choices, env)

let defApplied =
    applyArgs defender seargent
    |> normalize
    |> snd
    |> evalOp newEnv
let passRangeMap = 
    let alwaysPassRange = (Value(Float (0.<inch> |> float)))
    newEnv
    |> Map.add "Range" alwaysPassRange
    |> Map.add "Defender" defApplied 
let failRangeMap = 
    let alwaysFailRange = (Value(Float (50000.<inch> |> float)))
    newEnv
    |> Map.add "Range" alwaysFailRange
    |> Map.add "Defender" defApplied 
let (chc, attApplied) =  applyArgs attacker seargent |> normalize

let eval x op = getp x op |> evalOp passRangeMap

[<Tests>]
let tests =
    let pairs = List.zip stats seargent
    
    let expectedStd =
        let expectedDice n plus =
            [ 1..n ]
            |> List.map (fun i -> 
                   if i >= plus then Check(Check.Pass(Int 1)), 1.0
                   else Check(Check.Fail(Int 1)), 1.0)
            |> Distribution.countedCases
            |> Dist
            |> Value
        pairs |> List.map (function 
                     | key, IsDPlus(6, plus) -> key, expectedDice 6 plus
                     | key, IsDPlus(3, plus) -> key, expectedDice 3 plus
                     | key, op -> key, op)
    testList "Attacker Tests" [ test "Check Charge Range" { 
                                    let result = getp "Charge Range" attApplied |> evalOp (Map.add "Phase" (Value(Str "Assault")) passRangeMap)
                                    let expected =
                                        Distribution.takeN (Distribution.uniformDistribution [ 1..6 ]) 2 |> Distribution.map (List.sum >> Int)
                                    result ==? (Value(Dist(expected)))
                                }
                                test "Check Assault Range" { 
                                    let result = getp "Assault Range" attApplied |> evalOp (Map.add "Phase" (Value(Str "Assault")) passRangeMap)
                                    
                                    let expected =
                                        Distribution.takeN (Distribution.uniformDistribution [ 1..6 ]) 2 |> Distribution.map (List.sum
                                                                                                                              >> ((+) 6)
                                                                                                                              >> Int)
                                    result ==? (Value(Dist(expected)))
                                }
                                test "Check Hit Results" { 
                                    let result = getp "Hit Results" attApplied |> evalOp (Map.add "Phase" (Value(Str "Assault")) passRangeMap)
                                    
                                    let expected =
                                        Value(Dist { Probabilities =
                                                         [ (Check(Check.Pass(Int 2)), (4. / 9.))
                                                           (Tuple(Check(Check.Pass(Int 1)), Check(Check.Fail(Int 1))), (4. / 9.))
                                                           (Check(Check.Fail(Int 2)), (1. / 9.)) ] })
                                    result ==? expected
                                }
                                test "Check Hit Results out of range" { 
                                    let result = getp "Hit Results" attApplied |> evalOp (Map.add "Phase" (Value(Str "Assault")) failRangeMap)
                                    
                                    let expected =
                                        Value(Dist { Probabilities =
                                                         [ (Check(Check.Fail(Int 2)), (1.)) ] })
                                    result ==? expected
                                }
                                test "Check Wound Results" { 
                                    let result = getp "Wound Results" attApplied |> evalOp (Map.add "Phase" (Value(Str "Assault")) passRangeMap)
                                    
                                    let expected =
                                        Value(Dist { Probabilities =
                                                         [ (Check(Check.Pass(Int 2)), (1. / 9.))
                                                           (Tuple(Check(Check.Pass(Int 1)), Check(Check.Fail(Int 1))), (4. / 9.))
                                                           (Check(Check.Fail(Int 2)), (4. / 9.)) ] })
                                    result ==? expected
                                }
                                test "Check Wound Results out of range" { 
                                    let result = getp "Wound Results" attApplied |> evalOp (Map.add "Phase" (Value(Str "Assault")) failRangeMap)
                                    
                                    let expected =
                                        Value(Dist { Probabilities =
                                                         [ (Check(Check.Fail(Int 2)), (1.)) ] })
                                    result ==? expected
                                }
                                test "Check Psychic Test" { 
                                    let result = getp "Psychic Test" attApplied |> evalOp (Map.add "Phase" (Value(Str "Psychic")) passRangeMap)
                                    let expected =
                                        Distribution.takeN (Distribution.uniformDistribution [ 1..6 ]) 2 |> Distribution.map (List.sum >> Int)
                                    result ==? (Value(Dist(expected)))
                                }
                                expectedStd
                                |> List.map (fun (key, expected) -> test (sprintf "Check %s" key) { eval key attApplied ==? expected })
                                |> testList "Std eval Tests" ]
// let attackerLam = <@ 
//     let m = 6
//     let a = 5
//     ["M",m; "A",a] @>
// let meq = 
//     emptyOp
//     |> chargeRange
//     |> (vInt 5 |~> "M") 
// let test char = get char |> single |> count
// let hitAssault = 
//     get Attacker "A"
//     |> repeatOp (test "WS")
//     |> total 
//     |> bindOp Attacker "Assault"
// let shotsAssault = 
//     [Var(Attacker, "A"); Var(Attacker, "A")]
//     |> opList 
//     |> product
//     |> single
//     |> count
//     |> bindOp Attacker "Shots"
// let dPlus d v = Value(DPlus(d,v))
// let vInt i = Value(Int(i))
// let d6 = Value(Dice(D6))
// let chargeRange = [d6;d6] |> opList |> total |> bindOp Attacker "Charge Range"
// let meleeRange = [get Attacker "M"; chargeRange] |> opList |> total |> bindOp Attacker "Assault Range"
// let psychicTest = [d6;d6] |> opList |> total |> bindOp Attacker "Psychic Test"
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
