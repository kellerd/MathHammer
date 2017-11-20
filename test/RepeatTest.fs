module RepeatTests
open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.Models.State
open FsCheckGen


let (==?) x y = Expect.equal x y ""
let es x op = get x |> op |> evalOp standardCall Map.empty<_,_> 
let ea x op = get x |> op |> evalOp avgCall Map.empty<_,_> 
let e x op = get x |> op |> evalOp sampleCall Map.empty<_,_> 


[<Tests>]
let tests = 
    let ``Repeating an operation gives correct length`` (FsCheck.NonNegativeInt x) op = 
        let a = vInt x
        let repeat = repeatOp op a
        let (ParamArray(result))= repeat |> evalOp standardCall Map.empty<_,_>
        Expect.equal (List.length result) (max x 0) "Length of repeat should be same as length input, or 0 if < 1"
        match result with 
        | [] -> ()
        | head::tail -> Expect.allEqual tail head "All elements in repeat should be the same"
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
        (result |> es "TotalSum") ==? (product |> es "TotalSum")
        (result |> ea "TotalSum") ==? (product |> ea "TotalSum")
        (result |> e  "TotalSum") ==? (product |> e  "TotalSum")

    testList "Repeat Tests" [
        ftestPropertyWithConfig (3,623) config "Repeating an operation gives correct length" ``Repeating an operation gives correct length``
        ftestPropertyWithConfig (3,623) config "Total - Repeat Op X Times equivalent to List init X Times "   <| ``Repeat is same as List.init`` Total
        ftestPropertyWithConfig (3,623) config "Product - Repeat Op X Times equivalent to List init X Times " <| ``Repeat is same as List.init`` Product
        ftestPropertyWithConfig  (3,623) config "Count - Repeat Op X Times equivalent to List init X Times "   <| ``Repeat is same as List.init`` Count
        ftestPropertyWithConfig  (3,623) config "Total(Repeat x y) = Product(x,y)" ``Repeat Sum is the same as product``]


// D6+3  = Scalara + Scalara Dist = Scalara Dist
// 3D6 = Repeat D6 3 = Scalara x Scalarb Dist = Scalarb Dist List
// Double D6 = Scalara x Scalara Dist = Scalara Dist
// D3D6s = Repeat D6 D3 = Scalara Dist x ScalarbDist = Dist Scalarb Dist List
// 3 + 3 = Scalar + Scalar = Scalar
// 3 × 3 = Scalar × Scalar = Scalar 
// 3 x 3 = Repeat 3 3 = Scalar x Scalar = Scalar list
// D6 3 = Repeat 3 D6 = Scalarb Dist x Scalara Dist Scalara List