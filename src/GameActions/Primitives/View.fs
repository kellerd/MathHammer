module GameActions.Primitives.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types


let alternateRoot model dispatch =
    let rec displayOperation operation = 
        match operation with 
        | Product (ops) -> str ""
        | DPlus (_,i) -> str ""
        | Total (OpList(ops)) when List.distinct ops = [Value(Dice(D3))] -> str ""
        | Total (OpList(ops)) when List.distinct ops = [Value(Dice(D6))] -> str ""
        | Total (ops)  -> str ""
        | Value(Dice(i))-> str ""
        | Value(Int(i)) -> str ""
        | NoValue -> span [Style [BorderStyle "dotted"; MinWidth 50;MinHeight 50]] []
        | Count(_) -> str ""
        | Value(_) ->     str ""
        | Var(_) ->    str ""
        | Let(_) -> str ""
    displayOperation model  

let root model dispatch =
    let rec displayManyOp ops =
        match ops with
        | OpList ops -> List.map displayOperation ops |> String.concat " + "
        | Unfold (op,op2) -> sprintf "%s, %s times" (displayOperation op) (displayOperation op2)
    and displayOperation operation = 
        match operation with 
        | Product (OpList(ops)) when List.distinct ops = [Value(Dice(D6))] -> sprintf "Product(%dD6)" (List.length ops)
        | Product (OpList(ops)) when List.distinct ops = [Value(Dice(D3))] -> sprintf "Product(%dD3)" (List.length ops)
        | Product (ops)  -> sprintf "Product(%s)" (displayManyOp ops)
        | Total (OpList(ops)) when List.distinct ops = [Value(Dice(D6))] -> sprintf "Total(%dD6)" (List.length ops)
        | Total (OpList(ops)) when List.distinct ops = [Value(Dice(D3))] -> sprintf "Total(%dD3)" (List.length ops)
        | Total (ops)  -> sprintf "Total(%s)" (displayManyOp ops)
        | Value(Dice(i))-> string i
        | Value(Int(i)) -> string i
        | NoValue -> "--"
        | DPlus (D6,i) -> string i + "+"
        | DPlus (D3,i) -> string i + "+ on D3"
        | DPlus(Reroll(is,D6), i) -> sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is))
        | DPlus(Reroll(is,D3), i) -> sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is))
        | DPlus(Reroll(is,Reroll(is2,d)), i) -> displayOperation (DPlus(Reroll(List.distinct (is @ is2),d),i))
        | Count(ops) -> sprintf "(Passes,Fails) in (%s)" (displayManyOp ops)
        | Var(env, str) -> match env with 
                           | Attacker -> sprintf "Attacker(%s)" str
                           | Defender -> sprintf "Target(%s)" str
                           | Global -> sprintf "Global(%s)" str
        | Let(env, str, op) ->  
            match env with 
            
            | Attacker -> sprintf "%s"  (displayOperation op)
            | Defender -> sprintf "%s" (displayOperation op)
            | Global -> sprintf "%s" (displayOperation op)
    displayOperation model  
    |> str

