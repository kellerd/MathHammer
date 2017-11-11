module FsCheckGen
open GameActions.Primitives.Types
open Expecto
open GameActions.Primitives.State
open MathHammer.Models.State
let rec (|IsValidGp|) (gp:GamePrimitive) : bool = 
    let rec isValid gp currentlyValid = 
        if currentlyValid then 
            match gp with 
            | Float(f) -> (System.Double.IsInfinity(f) || System.Double.IsNaN(f)) |> not
            | Int(_) -> true
            | Str null -> false
            | Str _ -> currentlyValid 
            | Check(Check.Pass(p)) -> isValid p true 
            | Check(Check.Fail(p)) -> isValid p true 
            | Check(Check.Tuple(p,p2)) -> isValid p (isValid p2 true) 
            | Check(Check.List(l)) -> List.forall (fun gp -> isValid (Check gp) true) l
            | NoValue -> true
            | Dist(d) -> 
                match d with 
                | [] -> true
                | ds -> 
                    let total = (List.sumBy snd ds)
                    List.forall (fun (a,p) -> isValid a (p >= 0.0 && p <= 1.0)) ds && (total >= 0.0 && total <= 1.0)
        else false  
    isValid gp true      
let (|IsValidDie|) = function
    | D6 | D3 -> true
    | Reroll (rs, D3) -> List.forall(fun i -> i = 1 || i = 2 || i = 3) rs
    | Reroll (rs, D6) -> List.forall(fun i -> i = 1 || i = 2 || i = 3 || i = 4 || i = 5 || i = 6) rs
    | _ -> false
let (|IsValidOp|) (op:Operation) : bool = 
    let rec isValid op currentlyValid = 
        if currentlyValid then 
            match op with 
            | Value (IsValidGp(v)) -> v 
            | Call _ -> true
            | PropertyGet(n, op) -> isValid op (isNull n |> not)
            | ParamArray ops -> List.foldBack (isValid) ops true
            | Var n -> isNull n |> not 
            | App(Lam(n, op),value) -> isValid op (isValid value (isNull n |> not))
            | App(f,value) -> 
                let v = (evalOp standardCall Map.empty<_,_> value)
                let c = (evalOp standardCall Map.empty<_,_> f)
                if isValid v true then
                    match c,v with 
                    | Lam(x, op),v -> isValid (App(Lam(x, op),v)) true
                    | Call(Dice(IsValidDie d)), Value(NoValue) -> d
                    | Call(Total ),       ParamArray ops     -> List.forall (fun op -> isValid op true) ops
                    | Call(Product),      ParamArray ops     -> List.forall (fun op -> isValid op true) ops
                    | Call(Count),        ParamArray ops     -> List.forall (fun op -> isValid op true) ops
                    | Call(GreaterThan),  ParamArray([Value(IsValidGp(gp));Value(IsValidGp(gp2))]) -> gp && gp2
                    | Call(Equals),       ParamArray([Value(IsValidGp(gp));Value(IsValidGp(gp2))]) ->  gp && gp2
                    | Call(LessThan),     ParamArray([Value(IsValidGp(gp));Value(IsValidGp(gp2))]) ->  gp && gp2
                    | Call(NotEquals),    ParamArray([Value(IsValidGp(gp));Value(IsValidGp(gp2))]) ->  gp && gp2
                    | Call(Repeat),       ParamArray([Lam(n, body);op2]) -> isValid op2 (isValid body (System.String.IsNullOrWhiteSpace n |> not))
                    | Call(Total),    Value(IsValidGp gp)
                    | Call(Product),  Value(IsValidGp gp) 
                    | Call(Count),    Value(IsValidGp gp) -> gp
                    | _ -> false
                else false 
            | Lam(n, body) -> isValid body (isNull n |> not)
            | Let(n, value, body) -> isValid body (isValid value (isNull n |> not))
            | IfThenElse(ifExpr, thenExpr, Some(elseExpr)) -> isValid ifExpr (isValid thenExpr (isValid elseExpr true))
            | IfThenElse(ifExpr, thenExpr, None) -> isValid ifExpr (isValid thenExpr true)
        else false 
    isValid op true


type GamePrimitiveGen() = static member GamePrimitive() : FsCheck.Arbitrary<GamePrimitive> = FsCheck.Arb.Default.Derive ()  |> FsCheck.Arb.filter ((|IsValidGp|))
type OperationGen() = static member Operation() : FsCheck.Arbitrary<Operation> = FsCheck.Arb.Default.Derive () |> FsCheck.Arb.filter (fun x -> (|IsValidOp|) x)
type DieGen() = static member Die() : FsCheck.Arbitrary<Die> = FsCheck.Arb.Default.Derive ()  |> FsCheck.Arb.filter ((|IsValidDie|))
let config = {FsCheckConfig.defaultConfig with arbitrary = (typeof<OperationGen>)::(typeof<GamePrimitiveGen>)::(typeof<DieGen>)::FsCheckConfig.defaultConfig.arbitrary}