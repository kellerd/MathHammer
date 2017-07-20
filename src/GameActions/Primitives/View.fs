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
and unparseValue = function   
    | Dice(i) -> string i
    | Int(i) -> string i
    | ManyOp(m) -> sprintf "(%s)" <|displayManyOp m
    | NoValue -> "--"
    | DPlus (D6,i) -> string i + "+"
    | DPlus (D3,i) -> string i + "+ on D3"
    | DPlus(Reroll(is,D6), i) -> sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is))
    | DPlus(Reroll(is,D3), i) -> sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is))
    | DPlus(Reroll(is,Reroll(is2,d)), i) -> unparse <| Value(DPlus(Reroll(List.distinct (is @ is2),d),i)) 
and unparse operation = 
    match operation with 
    | Call f -> unparseCall f
    | Value(v)-> unparseValue v
    | Var (scope,v) -> sprintf "%A.%s" scope v
    | Lam(sc,p,x) -> sprintf "fun %s -> %s" p (unparse x)
    | App(Lam(sc,p,x),a) -> paren (unparse (Lam(sc,p,x))) + " " + argstring a
    | App(f,a) -> unparse f + " " + argstring a
    | Let(env, str, op) ->  sprintf "%s" (unparse op)
and argstring = function 
    | Var (scope,v) -> sprintf "%A.%s" scope v
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
        | Lam(_, param, body) -> str ""
    displayOperation model  

let root model dispatch =
           
    unparse model  
    |> str

