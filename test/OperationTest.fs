module OperationTests
open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.Models.State
open Distribution
open ExpectoFsCheck
let (==?) x y = Expect.equal x y ""

let rec (|IsValidGp|) (gp:GamePrimitive) : bool = 
    match gp with 
    | Float(f) -> (System.Double.IsInfinity(f) || System.Double.IsNaN(f)) |> not
    | Int(_) -> true
    | Str null -> false
    | Str _ -> true 
    | Check(Check.Pass(IsValidGp(p))) -> p
    | Check(Check.Fail(IsValidGp(p))) -> p
    | Check(Check.Tuple(IsValidGp(p),IsValidGp(p2))) -> p && p2
    | Check(Check.List(l)) -> List.exists(Check >> (|IsValidGp|) >> not) l |> not
    | NoValue -> true
    | Dist(d) -> List.exists(fun (a,p) -> ((|IsValidGp|) (Float(p)) && (|IsValidGp|) a) |> not ) d |> not
type GamePrimitiveGen() =
   static member GamePrimitive() : FsCheck.Arbitrary<GamePrimitive> =
    // FsCheck.Gen.elements [Float(5.0);NoValue;Int(6);Float(nan)]
    // |> FsCheck.Arb.fromGen
    FsCheck.Arb.Default.Derive () 
    |> FsCheck.Arb.filter ((|IsValidGp|))
let config = {FsCheckConfig.defaultConfig with arbitrary = (typeof<GamePrimitiveGen>)::FsCheckConfig.defaultConfig.arbitrary}

[<Tests>]
let tests = 
    testList "Operation Tests" [
        let stdEval = normalizeOp >> evalOp standardCall Map.empty<_,_>
        yield test "Evalled D6 equal std distribution of integers, reversed" {
            let result = stdEval d6
            let expected = [1..6] |> List.map (Int) |> List.rev |> uniformDistribution |> Dist |> Value
            result ==? expected
        }
        
        yield test "Evalled D3 equal std distribution of integers, reversed" {
            let result = stdEval d3
            let expected = [1..3] |> List.map (Int) |> List.rev |> uniformDistribution |> Dist |> Value
            result ==? expected
        }
        //Let x = 3 returns 3 as well as binding to environment
        let retValueIsSame f v = 
            let evaled = Let("x", Value(v) ,Var ("x")) |> f |> evalOp standardCall Map.empty<_,_> 
            let evaled' = Value(v) |> f |> evalOp standardCall Map.empty<_,_> 
            evaled ==? evaled'
        yield testPropertyWithConfig config "let x = 3 returns 3 evaluated without normalization" (retValueIsSame id)
        yield testPropertyWithConfig config "let x = 3 returns 3 evaluated with normalization" (retValueIsSame normalizeOp)
        //Let x = some number in
        //x + some other number
        let addition x y  =    
            let result =
                Let("x", Value(y) ,App(Call Total, ParamArray([Value(x);Var ("x")])))
                |> evalOp standardCall Map.empty<_,_>
            let expected = Value(x + y)
            result ==? expected 
            
        yield testPropertyWithConfig config "Addition in child scope is valid" addition
        let totalOfXIsX x = 
            let expected = x |> Value
            let result =
                Let("x", expected ,App(Call Total, Var("x"))) 
                |> evalOp standardCall Map.empty<_,_>
            result ==? expected
        yield testPropertyWithConfig config "Total of x is x" totalOfXIsX
        let countOfOneXIsOneX x = 
            let v = Value(Check(Check.Pass(x)))
            let (Value(Dist(d))) =
                Let("x", v ,App(Call Count, v)) 
                |> evalOp standardCall Map.empty<_,_>
            let expected = always (Check(Check.Tuple (Int(1),Int(0))))
            d ==? expected
        yield testPropertyWithConfig config "Count of one passed result is 1"  countOfOneXIsOneX 

        let unfoldD6 x = 
            let ws = dPlus D6 3
            let a = vInt x
            let unfold = unfoldOp ws a
            let (ParamArray(result))= unfold |> evalOp standardCall Map.empty<_,_>
            Expect.equal (List.length result) (max x 0) "Length of unfold should be same as length input, or 0 if < 1"
            match result with 
            | [] -> ()
            | head::tail -> Expect.allEqual tail head "All elements in unfold should be the same"
        yield testPropertyWithConfig config "Unfold D6 by 3" unfoldD6 
    ]