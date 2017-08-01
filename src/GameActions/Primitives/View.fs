module GameActions.Primitives.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
let paren = sprintf "(%s)"

let rec displayManyOp ops =
    match ops with
    | OpList(ops) when List.distinct ops = [Value(Dice(D6))] -> sprintf "%dD6" (List.length ops)
    | OpList(ops) when List.distinct ops = [Value(Dice(D3))] -> sprintf "%dD3" (List.length ops)
    | OpList ops -> List.map unparse ops |> String.concat " + "
    | Unfold (op,op2) -> sprintf "%s, %s times" (unparse op) (unparse op2)
and unparseCall func = 
    match func with 
    | Count -> sprintf "(Passes,Fails) in " 
    | _  -> sprintf "%A" func 
and unparseResult = function 
    | Result.Pass(v) -> sprintf "Pass: %s" (unparseValue v)
    | Result.Fail(v) -> sprintf "Fail: %s" (unparseValue v)
    | Result.Tuple(v,v2) -> sprintf "(%s,%s)" (unparseValue v) (unparseValue v2)
    | Result.List(vs) -> sprintf "[%s]" (List.map (unparseResult) vs |> String.concat ";")    
and unparseValue = function   
    | Dice(i) -> string i
    | Int(i) -> string i
    | Dist(d) -> "distribution"
    | ManyOp(m) -> sprintf "(%s)" <|displayManyOp m
    | NoValue -> "--"
    | DPlus (D6,i) -> string i + "+"
    | DPlus (D3,i) -> string i + "+ on D3"
    | DPlus(Reroll(is,D6), i) -> sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is))
    | DPlus(Reroll(is,D3), i) -> sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is))
    | DPlus(Reroll(is,Reroll(is2,d)), i) -> unparse <| Value(DPlus(Reroll(List.distinct (is @ is2),d),i))
    | Str s -> s
    | Result(r) -> unparseResult r
    | Pair(v,v2) ->  sprintf "(%s,%s)" (unparseValue v) (unparseValue v2)
and unparse operation = 
    match operation with 
    | Call f -> unparseCall f
    | Value(v)-> unparseValue v
    | Var (v) -> sprintf "%s" v
    | Lam(p,x) -> sprintf "fun %s -> %s" p (unparse x)
    | App(Lam(p,x),a) -> paren (unparse (Lam(p,x))) + " " + argstring a
    | App(f,a) -> unparse f + " " + argstring a
    | Let(str, v, inner) ->  sprintf "%s" (unparse v)
and argstring = function 
    | Var (v) -> sprintf "%s" v
    | x -> paren (unparse x) 

let alternateRoot model dispatch =
    let rec displayOperation operation = 
        match operation with 
        | Value(ManyOp(OpList(ops)))  when List.distinct ops = [Value(Dice(D3))] -> str ""
        | Value(ManyOp(OpList(ops)))  when List.distinct ops = [Value(Dice(D6))] -> str ""
        | Value(DPlus (_,i)) -> str ""
        | Call Product -> str ""
        | Call Total  -> str ""
        | Value(Dice(i))-> str ""
        | Value(Int(i)) -> str ""
        | Value(NoValue) -> span [Style [BorderStyle "dotted"; MinWidth 50;MinHeight 50]] []
        | Call Count -> str ""
        | Value(_) ->     str ""
        | Var(_) ->    str ""
        | Let(_) -> str ""
        | App(f, value) -> str ""
        | Lam(param, body) -> str ""
    displayOperation model  

let root model dispatch =
           
    unparse model  
    |> str

