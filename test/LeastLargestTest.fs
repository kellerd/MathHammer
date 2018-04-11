module LeastLargestTest
open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State

let (==~) x y = 
    match x, y with 
    | Value(ParamArray ops), Value(ParamArray ops2) -> Expect.containsAll ops ops2 "" 
    | Value(gp), Value(gp2) -> Expect.equal gp gp2 ""
    | _ -> failwith "Did not return an array value, or static value"

[<Tests>]
let tests = 
    testList "Least Largest Avg Tests" [
        let eval = normalize >> snd >> evalOp Map.empty<_,_> 
        
        let largestTest data n = 
            let result =  data |> List.map Value |> largest n |> eval
            let sorted = List.sortDescending data 
            let expected = sorted |> Seq.truncate n |> Seq.toList |> List.map Value |> opList
            result ==~ expected

        let leastTest data n = 
            let result =  data |> List.map Value |> least n |> eval
            let sorted = List.sort data 
            let expected = sorted |> Seq.truncate n |> Seq.toList |> List.map Value |> opList
            result ==~ expected

        yield test "Median" {
            let result =  [Int 6;Int 7;Int 10; Int 8] |> List.map Value |> opList |> call Median |> eval
            let expected = Value(Float 7.5)
            result ==~ expected
        }
        yield test "Mean" {
            let result =  [Int 6;Int 7;Int 10; Int 8] |> List.map Value |> opList |> call Mean |> eval
            let expected = Value(Float 7.75)
            result ==~ expected
        }
        yield test "Mode" {
            let result =  [Int 6;Int 7;Int 10; Int 10; Int 8; Int 8] |> List.map Value |> opList |> call Mode |> eval
            let expected = [Int 10; Int 10; Int 8; Int 8] |> List.map Value |> opList
            result ==~ expected
        }
        yield test "Least a" { leastTest [Int 6;Int 7;Int 10; Int 8] 2 }
        yield test "Least b" { leastTest [Int 6;Int 7;Int 10; Int 10; Int 8; Int 8] 2 }
        yield test "Least c" { leastTest [Int 6;Int 7;Int 10; Int 8] 4 }
        yield test "Least d" { leastTest [Int 6;Int 7;Int 10; Int 8] 9 }
        yield test "Largest a" { largestTest [Int 6;Int 7;Int 10; Int 10; Int 8; Int 8] 2 }
        yield test "Largest b" { largestTest [Int 6;Int 7;Int 10; Int 10; Int 8; Int 8] 4 }
        yield test "Largest c" { largestTest [Int 6;Int 7;Int 10; Int 8] 4 }
        yield test "Largest d" { largestTest [Int 6;Int 7;Int 10; Int 10; Int 8; Int 8] 7 }
    ]