module GameActions.Primitives.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types




let root (model:Ability) dispatch =
    let rec displayOperation operation = 
        match operation with 
        | Many (op,count) -> sprintf "%d %s" count (displayOperation op)
        | DPlus (_,i) -> string i + "+"
        | Total (ops) when List.distinct ops = [Value(Dice(D6))] -> sprintf "Total(%dD6)" (List.length ops)
        | Total (ops) when List.distinct ops = [Value(Dice(D3))] -> sprintf "Total(%dD3)" (List.length ops)
        | Total (ops)  -> sprintf "Total(%s)" (List.map displayOperation ops |> String.concat " + ")
        | Value i -> string i
        | NoValue -> "--"
    displayOperation model  
    |> str
