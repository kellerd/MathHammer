module FsCheckGen
open GameActions.Primitives.Types
open Expecto
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