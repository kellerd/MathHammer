module DistTests
open Expecto                                       
open GameActions.Primitives.Types

[<Tests>]
let tests = 
    testList "Distribution Tests" [
        test "Check add only adds passes" {
            let setup = [1;2;3;4;5;6] 
                        |> List.map (fun i -> if i > 2 then i |> Int |> Check.Pass
                                              else i |> Int |> Check.Fail ) |> Distribution.uniformDistribution            
            let result = setup.Probabilities |> List.map fst |> List.reduce (Check.combineFavourPass (+))
            let expected = ([3..6] |> List.sum |> Int |> Check.Pass)
            Expect.equal result expected "Adding up dice is same as adding integers" 
        }
    ]
// let d3() = uniformDistribution [1..3]
// let d6() = uniformDistribution [1..6]
// let avg d = 
//     let result = List.sumBy(fun (a:int,b:float) -> float a * b) d  
//     result
// let improvement a b = 
//     let result = (avg a) - (avg b)
//     printfn "Improvement: %A" result;
//     result
// <@    
// let zoanthropeUnit isNeuroNear points = dist {

//     let! roll = d6()
//     let! roll2 = d6()
//     let reroll r = 
//         if r = 1 && isNeuroNear then d6()
//         else always r
//     let! reroll1 = reroll roll
//     let! reroll2 = reroll roll2
//     let total = reroll1 + reroll2
    
//     let! bonus = if points = 240 then always 3
//                  elif points > 120 then d3()
//                  else always 0
//     let! wounds = 
//         if total > 10 then d6() |> map ((+) bonus) 
//         elif total >= 5 then d3() |> map ((+) bonus) 
//         else always 0
//     return wounds                
// }
// 6
// @>
// let points units unitSize neuroThrope =
//     let n = if neuroThrope then 70 else 0
//     let total = units * unitSize * 40
//     (float n + float total)
// let check units unitSize neuroThrope =
    
//     let neuro = 
//         if neuroThrope then [zoanthropeUnit true 70]
//         else [] 
//     let result = 
//         List.init units (fun _ -> zoanthropeUnit neuroThrope (unitSize * 40)) @ neuro
//         |> List.sumBy avg
//     points units unitSize neuroThrope / result

// [
//     0,0,true
//     1,3,false
//     2,3,false
//     3,3,false
//     1,4,false
//     2,4,false
//     3,4,false
//     1,5,false
//     2,5,false
//     3,5,false
//     1,6,false
//     2,6,false
//     3,6,false
//     1,3,true
//     2,3,true
//     3,3,true
//     1,4,true
//     2,4,true
//     3,4,true
//     1,5,true
//     2,5,true
//     3,5,true
//     1,6,true
//     2,6,true
//     3,6,true
// ] |> List.map(fun (u,us,neur) -> (u,us,neur), check u us neur)
//   |> List.sortBy snd
//   |> List.take 12
//   |> List.iter (fun ((u,us,neur),eff) -> 
//         let pts = points u us neur
//         let totalWounds = (1. / eff) * pts
//         printfn "Units: %d UnitSize: %d Contains Neuro: %b Points per wound : %f Wounds : %f" u us neur  eff totalWounds
//    )
// check 1 4 true
// check 2 4 true
// check 3 4 true
// improvement (zoanthropeUnit true 120)  (zoanthropeUnit false 120) 
// improvement (zoanthropeUnit true 160) (zoanthropeUnit false 160)
// improvement (zoanthropeUnit true 240) (zoanthropeUnit false 240)

// improvement (zoanthropeUnit true 240) (zoanthropeUnit true 120)
// improvement (zoanthropeUnit false 240) (zoanthropeUnit false 120)

// (avg (zoanthropeUnit false 120) + avg (zoanthropeUnit false 120))  |> (/) 240.
// (avg (zoanthropeUnit false 240) )  |> (/) 240.

// (avg (zoanthropeUnit true 120) + avg (zoanthropeUnit true 120))  |> (/) 320.
// (avg (zoanthropeUnit true 160) + avg (zoanthropeUnit true 160))  |> (/) 320.
// (avg (zoanthropeUnit true 200) + avg (zoanthropeUnit true 200))  |> (/) 400.
// (avg (zoanthropeUnit true 240))  |> (/) 240.