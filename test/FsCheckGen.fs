module FsCheckGen
open GameActions.Primitives.Types
open Expecto
open GameActions.Primitives.State
open MathHammer.Models.State
open Elmish.React.Common
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


type Tree = Leaf of int | Branch of Tree * Tree
let genOp =
    let rec genOp' = function
        | 0 -> FsCheck.Gen.constant (Value(NoValue))
        | n when n>0 -> 
            let subtree = genOp' (n/2)
            FsCheck.Gen.oneof [ 
                FsCheck.Gen.map Value FsCheck.Arb.generate<GamePrimitive> 
                FsCheck.Gen.map2 (fun (FsCheck.NonEmptyString n) op -> PropertyGet(n, op)) FsCheck.Arb.generate<_>  subtree
                FsCheck.Gen.map ParamArray (FsCheck.Gen.listOf subtree)
                FsCheck.Gen.map (fun (FsCheck.NonEmptyString n) -> Var n) FsCheck.Arb.generate<_> 
                FsCheck.Gen.map3 (fun (FsCheck.NonEmptyString n) op value -> App(Lam(n, op),value)) FsCheck.Arb.generate<_> subtree subtree 
                FsCheck.Gen.map2 (fun (FsCheck.NonEmptyString n) body -> Lam(n, body)) FsCheck.Arb.generate<_> subtree
                FsCheck.Gen.map3 (fun (FsCheck.NonEmptyString n) value body -> Let(n, value, body)) FsCheck.Arb.generate<_> subtree subtree
                FsCheck.Gen.map3 (fun ifExpr thenExpr elseExpr -> IfThenElse(ifExpr, thenExpr, elseExpr)) subtree subtree (FsCheck.Gen.optionOf subtree)
                                        
                //FsCheck.Gen.map App(f,value) -> 
                //     let v = (evalOp standardCall Map.empty<_,_> value)
                //     let c = (evalOp standardCall Map.empty<_,_> f)
                //     if isValid v true then
                //         match c,v with 
                //         | Lam(x, op),v -> isValid (App(Lam(x, op),v)) true
                //         | Call(Dice(IsValidDie d)), Value(NoValue) -> d
                //         | Call(Total ),       ParamArray ops     -> List.forall (fun op -> isValid op true) ops
                //         | Call(Product),      ParamArray ops     -> List.forall (fun op -> isValid op true) ops
                //         | Call(Count),        ParamArray ops     -> List.forall (fun op -> isValid op true) ops
                //         | Call(GreaterThan),  ParamArray([Value(IsValidGp(gp));Value(IsValidGp(gp2))]) -> gp && gp2
                //         | Call(Equals),       ParamArray([Value(IsValidGp(gp));Value(IsValidGp(gp2))]) ->  gp && gp2
                //         | Call(LessThan),     ParamArray([Value(IsValidGp(gp));Value(IsValidGp(gp2))]) ->  gp && gp2
                //         | Call(NotEquals),    ParamArray([Value(IsValidGp(gp));Value(IsValidGp(gp2))]) ->  gp && gp2
                //         | Call(Repeat),       ParamArray([Lam(n, body);op2]) -> isValid op2 (isValid body (System.String.IsNullOrWhiteSpace n |> not))
                //         | Call(Total),    Value(IsValidGp gp)
                //         | Call(Product),  Value(IsValidGp gp) 
                //         | Call(Count),    Value(IsValidGp gp) -> gp
                //         | _ -> false
                //     else false 
                ]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    FsCheck.Gen.sized genOp'
//FsCheck.Gen.sample 1 genOp
type GamePrimitiveGen() = static member GamePrimitive() : FsCheck.Arbitrary<GamePrimitive> = FsCheck.Arb.Default.Derive ()  |> FsCheck.Arb.filter ((|IsValidGp|))
type DieGen() = static member Die() : FsCheck.Arbitrary<Die> = FsCheck.Arb.Default.Derive ()  |> FsCheck.Arb.filter ((|IsValidDie|))
type GenOp() = static member Operation() : FsCheck.Arbitrary<Operation> = FsCheck.Arb.fromGen genOp
let config = {FsCheckConfig.defaultConfig with arbitrary = (typeof<GenOp>)::(typeof<GamePrimitiveGen>)::(typeof<DieGen>)::FsCheckConfig.defaultConfig.arbitrary}