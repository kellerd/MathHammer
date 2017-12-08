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
let ea x op = get x |> op |> evalOp avgCall Map.empty<_,_> 
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
        | x -> failwith <| sprintf "Result is wrong type %A" x
    let ``Repeat is same as List.init`` functionToCall operation = 
        let passOrFailCount = operation |> List.map (Value) |> opList |> call Count
        let times = 3
        let ``Three Passes`` = List.init times (fun _ -> passOrFailCount) |> opList |> call functionToCall >>= "ThreePasses"
        let ``Three Passes Repeat`` = repeat (Lam("_", passOrFailCount)) (vInt times) |> call functionToCall >>= "ThreePasses"
        (es  "ThreePasses" ``Three Passes``) ==? (es "ThreePasses" ``Three Passes Repeat``)
        (ea  "ThreePasses" ``Three Passes``) ==? (ea "ThreePasses" ``Three Passes Repeat``)
        (e   "ThreePasses" ``Three Passes``) ==? (e  "ThreePasses" ``Three Passes Repeat``)

    let ``Repeat Sum is the same as product`` x y =
        let result = repeatOp x y |> call Total >>= "TotalSum" 
        let product = [x;y] |> opList |> call Product >>= "TotalProduct" 
        (result |> es "TotalSum") ==? (product |> es "TotalProduct")
        (result |> ea "TotalSum") ==? (product |> ea "TotalProduct")
        (result |> e  "TotalSum") ==? (product |> e  "TotalProduct")
    testList "Repeat Tests" [
        testPropertyWithConfig config "Repeating an operation gives correct length" ``Repeating an operation gives correct length``
        testPropertyWithConfig config "Total - Repeat Op X Times equivalent to List init X Times "   <| ``Repeat is same as List.init`` Total
        testPropertyWithConfig config "Product - Repeat Op X Times equivalent to List init X Times " <| ``Repeat is same as List.init`` Product
        testPropertyWithConfig config "Count - Repeat Op X Times equivalent to List init X Times "   <| ``Repeat is same as List.init`` Count
        ptestPropertyWithConfig config "Total(Repeat x y) = Product(x,y)" ``Repeat Sum is the same as product`` ]
