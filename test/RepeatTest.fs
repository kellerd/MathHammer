module RepeatTests
open FsCheckGen
open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.Models.State
let (==?) x y = 
    //printfn "%A" x
    //printfn "%A" y
    Expect.equal x y ""
let es x op = get x |> op |> evalOp standardCall Map.empty<_,_> 
//let ea x op = get x |> op |> evalOp avgCall Map.empty<_,_> 
let e x op = get x |> op |> evalOp sampleCall Map.empty<_,_> 

[<Tests>]
let tests = 
    let ``Repeating an operation gives correct length`` (FsCheck.NonNegativeInt x) op = 
        let a = vInt x
        let repeat = repeatOp op a
        let result = repeat |> evalOp standardCall Map.empty<_,_>
        match result with 
        | Value(ParamArray([])) -> Expect.equal x 0 "X should be 0 if an empty list is the result"
        | Value(ParamArray((head::tail) as result)) -> 
            Expect.equal (List.length result) (max x 0) "Length of repeat should be same as length input, or 0 if < 1"
            Expect.allEqual tail head "All elements in repeat should be the same"
        | x -> failtest <| sprintf "Result is wrong type %A" x
    let ``Repeat is same as List.init`` functionToCall operation = 
        let passOrFailCount = operation |> List.map (Value) |> opList |> call Count
        let times = 3
        let ``Three Passes`` = List.init times (fun _ -> passOrFailCount) |> opList |> call functionToCall >>= "ThreePasses"
        let ``Three Passes Repeat`` = repeat (Lam("_", passOrFailCount)) (vInt times) |> call functionToCall >>= "ThreePasses"
        (es  "ThreePasses" ``Three Passes``) ==? (es "ThreePasses" ``Three Passes Repeat``)
        //(ea  "ThreePasses" ``Three Passes``) ==? (ea "ThreePasses" ``Three Passes Repeat``)
        (e   "ThreePasses" ``Three Passes``) ==? (e  "ThreePasses" ``Three Passes Repeat``)


    // let x = repeatOp (vInt 4) (vInt 4) |> evalOp standardCall initialMap  
    // let x = repeatOp (d6) (vInt 4) |> evalOp standardCall initialMap  
    // let x = repeatOp (vInt 4) (d6)  |> evalOp standardCall initialMap 
    // let x = repeatOp (d6) (d6)  |> evalOp standardCall initialMap  
    // let x = repeatOp (vInt 4) (Value(ParamArray([vInt 3; vInt 2])))  |> evalOp standardCall initialMap 
    // let x = repeatOp (Value(ParamArray([vInt 3; vInt 2]))) (vInt 4)  |> evalOp standardCall initialMap 
    
    let ``Repeat Sum is the same as product`` (TwoSimilarTypes (x,y)) =
        let result =  
            repeatOp (Value x) (Value y) 
            |> call Total 
            |> call Total 
            >>= "TotalSum" 
        let product = 
            [Value x;Value y] 
            |> opList 
            |> call Product 
            >>= "TotalProduct" 
        let (sumStandard, productStandard) = (result |> es "TotalSum"), (product |> es "TotalProduct")
        sumStandard ==? productStandard
        
        let (sumSample, productSample) = (result |> e "TotalSum"), (product |> e "TotalProduct")
        sumSample ==? productSample
    testList "Repeat Tests" [
        testPropertyWithConfig config "Repeating an operation gives correct length" ``Repeating an operation gives correct length``
        testPropertyWithConfig config "Total - Repeat Op X Times equivalent to List init X Times "   <| ``Repeat is same as List.init`` Total
        testPropertyWithConfig config "Product - Repeat Op X Times equivalent to List init X Times " <| ``Repeat is same as List.init`` Product
        testPropertyWithConfig config "Count - Repeat Op X Times equivalent to List init X Times "   <| ``Repeat is same as List.init`` Count
        testPropertyWithConfig config "Total(Repeat x y) = Product(x,y)" ``Repeat Sum is the same as product`` ]
