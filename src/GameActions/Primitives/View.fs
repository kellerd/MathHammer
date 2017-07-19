module GameActions.Primitives.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
let paren = sprintf "(%s)"

let rec displayManyOp ops =
    match ops with
    | OpList ops -> List.map unparse ops |> String.concat " + "
    | Unfold (op,op2) -> sprintf "%s, %s times" (unparse op) (unparse op2)
and call func = 
    match func with 
    | Product (OpList(ops)) when List.distinct ops = [Value(Dice(D6))] -> sprintf "Product(%dD6)" (List.length ops)
    | Product (OpList(ops)) when List.distinct ops = [Value(Dice(D3))] -> sprintf "Product(%dD3)" (List.length ops)
    | Product (ops)  -> sprintf "Product(%s)" (displayManyOp ops)
    | Total (OpList(ops)) when List.distinct ops = [Value(Dice(D6))] -> sprintf "Total(%dD6)" (List.length ops)
    | Total (OpList(ops)) when List.distinct ops = [Value(Dice(D3))] -> sprintf "Total(%dD3)" (List.length ops)
    | Total (ops)  -> sprintf "Total(%s)" (displayManyOp ops)
    | Count(ops) -> sprintf "(Passes,Fails) in (%s)" (displayManyOp ops)
    | DPlus (D6,i) -> string i + "+"
    | DPlus (D3,i) -> string i + "+ on D3"
    | DPlus(Reroll(is,D6), i) -> sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is))
    | DPlus(Reroll(is,D3), i) -> sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is))
    | DPlus(Reroll(is,Reroll(is2,d)), i) -> unparse <| Call(DPlus(Reroll(List.distinct (is @ is2),d),i))
    
and unparse operation = 
    match operation with 
    | Call (f) -> call f
    | Value(Dice(i))-> string i
    | Value(Int(i)) -> string i
    | Value(NoValue) -> "--"
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
        | Call(DPlus (_,i)) -> str ""
        | Call(Product (ops)) -> str ""
        | Call(Total (OpList(ops))) when List.distinct ops = [Value(Dice(D3))] -> str ""
        | Call(Total (OpList(ops))) when List.distinct ops = [Value(Dice(D6))] -> str ""
        | Call(Total (ops)) -> str ""
        | Value(Dice(i))-> str ""
        | Value(Int(i)) -> str ""
        | Value(NoValue) -> span [Style [BorderStyle "dotted"; MinWidth 50;MinHeight 50]] []
        | Call(Count(_)) -> str ""
        | Value(_) ->     str ""
        | Var(_) ->    str ""
        | Let(_) -> str ""
        | App(f, value) -> str ""
        | Lam(_, param, body) -> str ""
    displayOperation model  

let root model dispatch =
           
    unparse model  
    |> str

