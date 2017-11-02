module DistTests
open Distribution
open Expecto                                       
open GameActions.Primitives.Types
let dist = [1;2;3;4;5;6] |> List.map (fun i -> if i > 2 then i |> Int |> Check.Pass
                                               else i |> Int |> Check.Fail ) |> uniformDistribution
[<Tests>]
let tests = 
    testList "Distribution Tests" [
        test "Check add only adds passes" {
            let result = dist |> List.map fst |> List.reduce (Check.add)
            let expected = ([3..6] |> List.sum |> Int |> Check.Pass)
            Expect.equal result expected "Adding up dice is same as adding integers" 
        }
    ]