#load "LoadModules.fsx"
open Check
open Distribution
open GameActions.Primitives.Types
let dist = [1;2;3;4;5;6] |> List.map (fun i -> if i > 2 then i |> Int |> Pass
                                               else i |> Int |> Fail ) |> uniformDistribution

dist |> List.map fst |> List.reduce (Check.add) = ([1..6] |> List.sum |> Int |> Pass)