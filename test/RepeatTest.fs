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
let rec (|IsScalar|_|) = function 
    | Int _ -> Some "Int"
    | Str(_) -> Some "Str"
    | Float(_) -> Some "Float"
    | Check(Check.CheckValue(IsScalar(c))) -> c
    | NoValue -> Some "NoValue"
    | ParamArray(_) -> None
    | Tuple(_) -> None
    | Dist(_) -> None

let es x op = get x |> op |> evalOp standardCall Map.empty<_,_> 
let ea x op = get x |> op |> evalOp avgCall Map.empty<_,_> 
let e x op = get x |> op |> evalOp sampleCall Map.empty<_,_> 


[<Tests>]
let tests = 
    let ``Repeating an operation gives correct length`` (FsCheck.NonNegativeInt x) op = 
        let a = vInt x
        let repeat = repeatOp op a
        let (Value(ParamArray(result)))= repeat |> evalOp standardCall Map.empty<_,_>
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
        let x = Int 6 |> Distribution.always |> Dist |> Value
        let y = noValue
        let result = repeatOp x y |> call Total >>= "TotalSum" 
        let product = [x;y] |> opList |> call Product >>= "TotalProduct" 
        (result |> es "TotalSum") ==? (product |> es "TotalProduct")
        (result |> ea "TotalSum") ==? (product |> ea "TotalProduct")
        (result |> e  "TotalSum") ==? (product |> e  "TotalProduct")
    
    let ``3 + 3 = Scalar + Scalar = Scalar`` (value1:GamePrimitive) (value2:GamePrimitive) = 
            let expected = Value( )
            let result = [value1;value2] >>= opList |> call Product >>= "result" |> es "result"
            result ==? expected
    let ``3 × 3 = Scalar × Scalar = Scalar `` (value1:GamePrimitive) (value2:GamePrimitive) = 
            let expected = Value( )
            let result = [value1;value2] >>= opList |> call Product >>= "result" |> es "result"
            result ==? expected
    let ``3 x 3 = Repeat 3 3 = Scalar x Scalar = Scalar list`` (value1:GamePrimitive) (value2:GamePrimitive) = 
            let expected = Value( )
            let result = [value1;value2] >>= opList |> call Product >>= "result" |> es "result"
            result ==? expected
    let ``3D6 = Repeat D6 3 = Scalara x Scalarb Dist = Scalarb Dist List`` (value1:GamePrimitive) (value2:GamePrimitive) = 
            let expected = Value( )
            let result = [value1;value2] >>= opList |> call Product >>= "result" |> es "result"
            result ==? expected
    let ``Double D6 = Scalara x Scalara Dist = Scalara Dist`` (value1:GamePrimitive) (value2:GamePrimitive) = 
            let expected = Value( )
            let result = [value1;value2] >>= opList |> call Product >>= "result" |> es "result"
            result ==? expected
    let ``D3D6s = Repeat D6 D3 = Scalara Dist x ScalarbDist = Dist Scalarb Dist List`` (value1:GamePrimitive) (value2:GamePrimitive) = 
            let expected = Value( )
            let result = [value1;value2] >>= opList |> call Product >>= "result" |> es "result"
            result ==? expected
    let ``D6 3 = Repeat 3 D6 = Scalarb Dist x Scalara Dist Scalara List`` (value1:GamePrimitive) (value2:GamePrimitive) = 
            let expected = Value( )
            let result = [value1;value2] >>= opList |> call Product >>= "result" |> es "result"
            result ==? expected
    
    testList "Repeat Tests" [
        testPropertyWithConfig config "Repeating an operation gives correct length" ``Repeating an operation gives correct length``
        testPropertyWithConfig config "Total - Repeat Op X Times equivalent to List init X Times "   <| ``Repeat is same as List.init`` Total
        testPropertyWithConfig config "Product - Repeat Op X Times equivalent to List init X Times " <| ``Repeat is same as List.init`` Product
        testPropertyWithConfig config "Count - Repeat Op X Times equivalent to List init X Times "   <| ``Repeat is same as List.init`` Count
        testPropertyWithConfig config "Total(Repeat x y) = Product(x,y)" ``Repeat Sum is the same as product``
        testPropertyWithConfig config "3 + 3 = Scalar + Scalar = Scalar" ``3 + 3 = Scalar + Scalar = Scalar``
        testPropertyWithConfig config "3 × 3 = Scalar × Scalar = Scalar " ``3 × 3 = Scalar × Scalar = Scalar ``
        testPropertyWithConfig config "3 x 3 = Repeat 3 3 = Scalar x Scalar = Scalar list" ``3 x 3 = Repeat 3 3 = Scalar x Scalar = Scalar list``
        testPropertyWithConfig config "3D6 = Repeat D6 3 = Scalara x Scalarb Dist = Scalarb Dist List" ``3D6 = Repeat D6 3 = Scalara x Scalarb Dist = Scalarb Dist List``
        testPropertyWithConfig config "Double D6 = Scalara x Scalara Dist = Scalara Dist" ``Double D6 = Scalara x Scalara Dist = Scalara Dist``
        testPropertyWithConfig config "D3D6s = Repeat D6 D3 = Scalara Dist x ScalarbDist = Dist Scalarb Dist List" ``D3D6s = Repeat D6 D3 = Scalara Dist x ScalarbDist = Dist Scalarb Dist List``
        testPropertyWithConfig config "D6 3 = Repeat 3 D6 = Scalarb Dist x Scalara Dist Scalara List" ``D6 3 = Repeat 3 D6 = Scalarb Dist x Scalara Dist Scalara List``
        ]
