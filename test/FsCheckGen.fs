module FsCheckGen
open GameActions.Primitives.Types
open Expecto
open GameActions.Primitives.State
open MathHammer.Models.State
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
let (|IsValidDie|) = function
    | D6 | D3 -> true
    | Reroll (rs, D3) -> List.exists(fun i -> i = 1 || i = 2 || i = 3) rs
    | Reroll (rs, D6) -> List.exists(fun i -> i = 1 || i = 2 || i = 3 || i = 4 || i = 5 || i = 6) rs
    | _ -> false
let (|IsValidOp|) (op:Operation) : bool = 
    let rec isValid op currentlyValid = 
        if currentlyValid then 
            match op with 
            // | Value (IsValidGp(v)) -> v 
            | Call _ -> true
            // | PropertyGet(n, op) -> isValid op (System.String.IsNullOrWhiteSpace n |> not)
            // | ParamArray ops -> List.foldBack (isValid) ops true
            | Var n -> System.String.IsNullOrWhiteSpace n |> not
            // | App(Lam(n, op),value) -> isValid op (isValid value (System.String.IsNullOrWhiteSpace n |> not))
            // | App(f,value) -> 
            //     let v = (evalOp standardCall Map.empty<_,_> value)
            //     let c = (evalOp standardCall Map.empty<_,_> f)
            //     if isValid v true then
            //         match c,v with 
                    // | Lam(x, op),v -> isValid (App(Lam(x, op),v)) true
                    // | Call(Dice(IsValidDie d)), Value(NoValue) -> d
                    // | Call(Total ),       ParamArray ops     -> List.forall (fun op -> isValid op true) ops
                    // | Call(Product),      ParamArray ops     -> List.forall (fun op -> isValid op true) ops
                    // | Call(Count),        ParamArray ops     -> List.forall (fun op -> isValid op true) ops
                    // | Call(GreaterThan),  ParamArray([Value(IsValidGp(gp));Value(IsValidGp(gp2))]) -> gp && gp2
                    // | Call(Equals),       ParamArray([Value(IsValidGp(gp));Value(IsValidGp(gp2))]) ->  gp && gp2
                    // | Call(LessThan),     ParamArray([Value(IsValidGp(gp));Value(IsValidGp(gp2))]) ->  gp && gp2
                    // | Call(NotEquals),    ParamArray([Value(IsValidGp(gp));Value(IsValidGp(gp2))]) ->  gp && gp2
                    // | Call(Repeat),       ParamArray([Lam(n, body);op2]) -> isValid op2 (isValid body (System.String.IsNullOrWhiteSpace n |> not))
                    // | Call(Total),    Value(IsValidGp gp)
                    // | Call(Product),  Value(IsValidGp gp) 
                    // | Call(Count),    Value(IsValidGp gp) -> gp
                //     | _ -> false
                // else false 
            // | Lam(n, body) -> isValid body (System.String.IsNullOrWhiteSpace n |> not)
            // | Let(n, value, body) -> isValid body (isValid value (System.String.IsNullOrWhiteSpace n |> not))
            // | IfThenElse(ifExpr, thenExpr, Some(elseExpr)) -> isValid ifExpr (isValid thenExpr (isValid elseExpr true))
            // | IfThenElse(ifExpr, thenExpr, None) -> isValid ifExpr (isValid thenExpr true)
            | _ -> true
        else false 
    isValid op true
type GamePrimitiveGen() = static member GamePrimitive() : FsCheck.Arbitrary<GamePrimitive> = FsCheck.Arb.Default.Derive ()  |> FsCheck.Arb.filter ((|IsValidGp|))
type OperationGen() = static member Operation() : FsCheck.Arbitrary<Operation> = FsCheck.Arb.Default.Derive ()  |> FsCheck.Arb.filter ((|IsValidOp|))
type DieGen() = static member Die() : FsCheck.Arbitrary<Die> = FsCheck.Arb.Default.Derive ()  |> FsCheck.Arb.filter ((|IsValidDie|))
let config = {FsCheckConfig.defaultConfig with arbitrary = (typeof<GamePrimitiveGen>)::(typeof<DieGen>)::FsCheckConfig.defaultConfig.arbitrary}