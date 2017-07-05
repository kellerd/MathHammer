module GameActions.Primitives.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types


let alternateRoot (model:Ability) dispatch =
    let rec displayOperation operation = 
        match operation with 
        | Multiply (op,op2) -> str ""
        | DPlus (_,i) -> str ""
        | Total (ops) when List.distinct ops = [Value(Dice(D3))] -> str ""
        | Total (ops) when List.distinct ops = [Value(Dice(D6))] -> str ""
        | Total (ops)  -> str ""
        | Value(Dice(i))-> str ""
        | Value(Int(i)) -> str ""
        | NoValue -> span [Style [BorderStyle "dotted"; MinWidth 50;MinHeight 50]] []
        | Count(_) -> str ""
        | Value(_) ->     str ""
        | Var(_) ->    str ""
        | Let(_) -> str ""
    displayOperation model  

let root (model:Ability) dispatch =
    let rec displayOperation operation = 
        match operation with 
        | Multiply (op,op2) -> sprintf "%s * %s" (displayOperation op) (displayOperation op2)
        | Total (ops) when List.distinct ops = [Value(Dice(D6))] -> sprintf "Total(%dD6)" (List.length ops)
        | Total (ops) when List.distinct ops = [Value(Dice(D3))] -> sprintf "Total(%dD3)" (List.length ops)
        | Total (ops)  -> sprintf "Total(%s)" (List.map displayOperation ops |> String.concat " + ")
        | Value(Dice(i))-> string i
        | Value(Int(i)) -> string i
        | NoValue -> "--"
        | DPlus (D6,i) -> string i + "+"
        | DPlus (D3,i) -> string i + "+ on D3"
        | DPlus(Reroll(is,D6), i) -> sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is))
        | DPlus(Reroll(is,D3), i) -> sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is))
        | DPlus(Reroll(is,Reroll(is2,d)), i) -> displayOperation (DPlus(Reroll(List.distinct (is @ is2),d),i))
        | Count(ops) -> sprintf "(Passes,Fails) in (%s)" (List.map displayOperation ops |> String.concat ",")
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
