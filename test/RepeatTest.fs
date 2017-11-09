module RepeatTest
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
    let repeatVsStraight = 
        let passOrFailCount = [dPlus D6 3] |> opList |> call Count
        let times = 3
        let ``Three Passes`` = List.init times (fun _ -> passOrFailCount) |> opList |> call Total >>= "ThreePasses"
        let ``Three Passes Repeat`` = repeat (Lam("_", passOrFailCount)) (vInt times) |> call Total >>= "ThreePasses"
        test "Straight Array should be the same as repeat" {
            (es  "ThreePasses" ``Three Passes``) ==? (es "ThreePasses" ``Three Passes Repeat``)
        }
    testList "Repeat Tests" [
        let repeatD6 x = 
            let ws = dPlus D6 3 
            let a = vInt x
            let repeat = repeatOp ws a
            let (ParamArray(result))= repeat |> evalOp standardCall Map.empty<_,_>
            Expect.equal (List.length result) (max x 0) "Length of repeat should be same as length input, or 0 if < 1"
            match result with 
            | [] -> ()
            | head::tail -> Expect.allEqual tail head "All elements in repeat should be the same"
        yield testPropertyWithConfig config "Repeat D6 by 3" repeatD6
        yield repeatVsStraight ]


// D6+3  = Scalara + Scalara Dist = Scalara Dist
// 3D6 = Repeat D6 3 = Scalara x Scalarb Dist = Scalarb Dist List
// Double D6 = Scalara x Scalara Dist = Scalara Dist
// D3D6s = Repeat D6 D3 = Scalara Dist x ScalarbDist = Dist Scalarb Dist List
// 3 + 3 = Scalar + Scalar = Scalar
// 3 × 3 = Scalar × Scalar = Scalar 
// 3 x 3 = Repear 3 3 = Scalar x Scalar = Scalar list
// D6 3 = Repeat 3 D6 = Scalarb Dist x Scalara Dist Scalara List
// Total(Repeat x y) = Product(x,y)