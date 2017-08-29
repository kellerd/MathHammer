module GameActions.Primitives.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
let paren react = div [] <| str "(" :: react @ [str ")"]

let rec displayManyOp ops =
    match ops with
    | OpList(ops) when List.distinct ops = [Value(Dice(D6))] -> sprintf "%dD6" (List.length ops) |> str
    | OpList(ops) when List.distinct ops = [Value(Dice(D3))] -> sprintf "%dD3" (List.length ops) |> str
    | OpList ops -> List.map unparse ops |> List.reduce (fun l1 l2 -> l1 @ (str " + " :: l2)) |> div []
and unparseCall func = 
    match func with 
    | Count -> sprintf "(Passes,Fails) in " |> str
    | _  -> sprintf "%A" func |> str
and unparseResult = function 
    | Result.Pass(v) -> div [Style [Color (Probability.View.colour 255.) ]] [str "Pass: "; (unparseValue v)]
    | Result.Fail(v) -> div [Style [Color (Probability.View.colour 0.) ]] [str  "Fail: "; (unparseValue v)]
    | Result.Tuple(v,v2) -> div [] [str "(" ; (unparseValue v); str ","; (unparseValue v2 ); str ")"]
    | Result.List(vs) -> div [] [
                                yield str "["
                                for result in vs do
                                    yield unparseResult result
                                    yield str ";" 
                                yield str "]"]
and unparseValue : GamePrimitive -> Fable.Import.React.ReactElement = function   
    | Dice(i) -> "D" + string i  |> str
    | Int(i) -> string i |> str
    | Dist(d) -> "distribution" |> str
    | ManyOp(m) -> div [] [str "("; displayManyOp m; str ")"]
    | NoValue -> "--" |> str
    | Str s -> s |> str
    | Result(r) -> unparseResult r
    | Float(f) -> sprintf "%.2f" f |> str
and unparse operation : Fable.Import.React.ReactElement list = 
    match operation with 
    | Call f -> [unparseCall f]
    | Value(v)-> [unparseValue v]
    | Var (v) -> [sprintf "%s" v |> str]
    | Lam(p,x) -> []
    | App(Call(GreaterThan),  Value(ManyOp(OpList [Value(Dice(D6)); Value(Int(i))]))) ->  [string i + "+" |> str]
    | App(Call(GreaterThan),  Value(ManyOp(OpList [Value(Dice(D3)); Value(Int(i))]))) ->  [string i + "+ on D3" |> str]
    | App(Call(GreaterThan),  Value(ManyOp(OpList [Value(Dice(Reroll(is,D6))); Value(Int(i))]))) ->  [sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is)) |> str]
    | App(Call(GreaterThan),  Value(ManyOp(OpList [Value(Dice(Reroll(is,D3))); Value(Int(i))]))) ->  [sprintf "%d+ rerolling (%s)"  i (String.concat "," (List.map string is)) |> str]
    | App(Call(GreaterThan),  Value(ManyOp(OpList [Value(Dice(Reroll(is,Reroll(is2,d)))); Value(Int(i))]))) ->  [unparse (App(Call(GreaterThan),  Value(ManyOp(OpList [Value(Dice(Reroll(List.distinct (is @ is2),d))); Value(Int(i))])))) |> div []]
    | App(Lam(p,x),a) -> unparse x //paren (unparse (Lam(p,x))) + " " + argstring a
    | App(f,a) -> unparse f @ [argstring a]
    | Let(v, value, inner) ->  (div [] ((b [] [str v]) :: unparse value)) :: (unparse inner)
and argstring = function 
    | Var (v) -> sprintf "%s" v |> str
    | x -> paren (unparse x) 

let alternateRoot model dispatch =
    let rec displayOperation operation = 
        match operation with 
        | Value(ManyOp(OpList(ops)))  when List.distinct ops = [Value(Dice(D3))] -> str ""
        | Value(ManyOp(OpList(ops)))  when List.distinct ops = [Value(Dice(D6))] -> str ""
        | Call Product -> str ""
        | Call Total  -> str ""
        | Call Unfold  -> str ""
        | Call Count -> str ""
        | Call GreaterThan -> str ""
        | Call LessThan -> str ""
        | Call Equals -> str ""
        | Call NotEquals -> str ""
        | Call Or -> str ""
        | Call And -> str ""
        | Value(Dice(i))-> str ""
        | Value(Int(i)) -> str ""
        | Value(NoValue) -> span [Style [BorderStyle "dotted"; MinWidth 50;MinHeight 50]] []
        | Value(_) ->     str ""
        | Var(_) ->    str ""
        | Let(_) -> str ""
        | App(f, value) -> str ""
        | Lam(param, body) -> str ""
    displayOperation model  

let root model dispatch =
           
    unparse model

