module GameActions.Primitives.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
let paren react = div [] <| str "(" :: react @ [str ")"]

let rec unparseResult unparseV = function 
    | Result.Pass(v) -> div [Style [Color (Probability.View.colour 255.) ]] [str "Pass: "; (unparseV v)]
    | Result.Fail(v) -> div [Style [Color (Probability.View.colour 0.) ]] [str  "Fail: "; (unparseV v)]
    | Result.Tuple(v,v2) -> div [] [str "(" ; (unparseV v); str ","; (unparseV v2 ); str ")"]
    | Result.List(vs) -> div [] [
                                yield str "["
                                for result in vs do
                                    yield unparseResult unparseV result
                                    yield str ";" 
                                yield str "]"]

let rec displayParamArray ops =
    match ops with
    | OpList(ops) when List.distinct ops = [GameActions.Primitives.State.``D#`` D6] -> sprintf "%dD6" (List.length ops) |> str
    | OpList(ops) when List.distinct ops = [GameActions.Primitives.State.``D#`` D3] -> sprintf "%dD3" (List.length ops) |> str
    | OpList ops -> List.map unparse ops |> List.reduce (fun l1 l2 -> l1 @ (str " + " :: l2)) |> div []
and unparseCall func = 
    match func with 
    | Dice(i) -> "D" + string i  |> str
    | Count -> sprintf "(Passes,Fails) in " |> str
    | _  -> sprintf "%A" func |> str
and unparseValue : GamePrimitive -> Fable.Import.React.ReactElement = function   
    | Int(i) -> string i |> str
    | Float(f) -> sprintf "%.2f" f |> str
    | Dist(d) -> "distribution" |> str
    | NoValue -> "--" |> str
    | Str s -> s |> str
    | Result r -> unparseResult unparseValue r
and unparse operation : Fable.Import.React.ReactElement list = 
    match operation with 
    | Call f -> [unparseCall f]
    | Value(v)-> [unparseValue v]
    | Var (v) -> [sprintf "%s" v |> str]
    | Lam(p,x) -> []
    | ParamArray(m) -> [str "("; displayParamArray m; str ")"]
    | App(Call(GreaterThan),  ParamArray(OpList [App(Call(Dice(D6)),noValue); Value(Int(i))])) ->  [string i + "+" |> str]
    | App(Call(GreaterThan),  ParamArray(OpList [App(Call(Dice(D3)),noValue); Value(Int(i))])) ->  [string i + "+ on D3" |> str]
    | App(Call(GreaterThan),  ParamArray(OpList [App(Call(Dice(Reroll(is,D6))),noValue); Value(Int(i))])) ->  [sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is)) |> str]
    | App(Call(GreaterThan),  ParamArray(OpList [App(Call(Dice(Reroll(is,D3))),noValue); Value(Int(i))])) ->  [sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is)) |> str]
    | App(Call(GreaterThan),  ParamArray(OpList [App(Call(Dice(Reroll(is,Reroll(is2,d)))),noValue); Value(Int(i))])) ->  
        [unparse (App(Call(GreaterThan),  ParamArray(OpList [App(Call(Dice(Reroll(List.distinct (is @ is2),d))),noValue); Value(Int(i))]))) |> div []]
    | App(Lam(p,x),a) -> unparse x //paren (unparse (Lam(p,x))) + " " + argstring a
    | App(f,a) -> unparse f @ [argstring a]
    | Let(v, value, inner) ->  (div [] ((b [] [str v]) :: unparse value)) :: (unparse inner)
and argstring = function 
    | Var (v) -> sprintf "%s" v |> str
    | x -> paren (unparse x) 

let alternateRoot model dispatch =
    let rec displayOperation operation = 
        match operation with 
        | ParamArray(OpList(ops))  when List.distinct ops = [GameActions.Primitives.State.``D#`` D3] -> str ""
        | ParamArray(OpList(ops))  when List.distinct ops = [GameActions.Primitives.State.``D#`` D6] -> str ""
        | Call Product -> str ""
        | Call Total  -> str ""
        | Call Unfold  -> str ""
        | Call (Dice _)  -> str ""
        | Call Count -> str ""
        | Call GreaterThan -> str ""
        | Call LessThan -> str ""
        | Call Equals -> str ""
        | Call NotEquals -> str ""
        | Call Or -> str ""
        | Call And -> str ""
        | Value(Int(i)) -> str ""
        | Value(NoValue) -> span [Style [BorderStyle "dotted"; MinWidth 50;MinHeight 50]] []
        | Value(_) ->     str ""
        | Var(_) ->    str ""
        | Let(_) -> str ""
        | App(f, value) -> str ""
        | Lam(param, body) -> str ""
        | ParamArray _ -> str ""
    displayOperation model  

let root model dispatch =
           
    unparse model

