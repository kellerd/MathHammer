module GameActions.Primitives.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Result
open Distribution
open Probability.View

let paren react = div [] <| str "(-" :: react @ [str ")"]

let rec unparseResult unparseV = function 
    | Result.Pass(v) -> div [Style [Color (Probability.View.colour 255.) ]] [str "Pass: "; (unparseV v)]
    | Result.Fail(v) -> div [Style [Color (Probability.View.colour 0.) ]] [str  "Fail: "; (unparseV v)]
    | Result.Tuple(v,v2) -> div [] [paren <| unparseV v::(str ",")::[unparseV v2]]
    | Result.List(vs) -> div [] [
                                yield str "["
                                for result in vs do
                                    yield unparseResult unparseV result
                                    yield str ";" 
                                yield str "]"]
     
      
let rec displayParamArray ops =
    match ops with
    | ops when List.distinct ops = [GameActions.Primitives.State.``D#`` D6] -> sprintf "%dD6" (List.length ops) |> str
    | ops when List.distinct ops = [GameActions.Primitives.State.``D#`` D3] -> sprintf "%dD3" (List.length ops) |> str
    | ops -> section [ClassName "columns"] (List.choose (fun op -> 
        match unparse op with 
        | [] -> None 
        | xs -> div [ClassName "column"] xs |> Some) ops)
    //| ops -> List.map unparse ops |> List.reduce (fun l1 l2 -> l1 @ (str " + " :: l2)) |> div []
and unparseCall func = 
    match func with 
    | Dice(i) -> "D" + string i  |> str
    | Count -> sprintf "(Passes,Fails) in " |> str
    | _  -> sprintf "%A" func |> str
and unparseDist (dist:Distribution.Distribution<GamePrimitive>) = 
    let result = 
          dist 
          |> List.groupBy fst
          |> List.map(fun (f,probs) -> f, List.sumBy snd probs)
    match result with 
    | [] -> str ""
    | _ ->  let max = result |> List.maxBy snd |> snd
            let min = result |> List.minBy snd |> snd
            let total = result |> List.sumBy snd 
            result
            |> List.map (fun (r, prob) -> 
                  match r with 
                  | IntResult r -> 
                      let rec passFailToExpectation = function 
                        | Pass _ -> float 0xFF  
                        | Fail _ -> float 0x00 
                        | List xs -> List.averageBy passFailToExpectation xs 
                        | Tuple (x,y) when x = y ->  float 0xFF 
                        | Tuple (x,y) -> (x / (x + y) ) * float 0xFF
                      let greenValue = Result.map float r |> passFailToExpectation
                      let alpha = opacity min max prob
                      div [Style [Color (colourA greenValue alpha)]] [str (printResultD r); str <| sprintf " %.1f%%" (prob / total * 100.)]
                  | v -> div [] [unparseValue v; str <| sprintf " %.1f%%" (prob / total * 100.)])
            |> div []       
and unparseValue : GamePrimitive -> Fable.Import.React.ReactElement = function   
    | Int(i) -> string i |> str
    | Float(f) -> sprintf "%.1f" f |> str
    | Dist(d) -> unparseDist d
    | NoValue -> "--" |> str
    | Str s -> b  [] [str s]
    | Result r -> unparseResult unparseValue r
and unparse operation : Fable.Import.React.ReactElement list = 
    match operation with 
    | Call f -> [unparseCall f]
    | Value(v)-> [unparseValue v]
    | Var (v) -> [sprintf "%s" v |> str]
    | Lam(p,x) -> []
    | ParamArray ([Value (Str _); Value(NoValue)]) -> []
    | ParamArray ([Value (Str _); Var _]) -> []
    | ParamArray(m) -> [displayParamArray m]
    | App(Call(GreaterThan),  ParamArray([App(Call(Dice(D6)),noValue); Value(Int(i))])) ->  [string (i+1) + "+" |> str]
    | App(Call(GreaterThan),  ParamArray([App(Call(Dice(D3)),noValue); Value(Int(i))])) ->  [string (i+1) + "+ on D3" |> str]
    | App(Call(GreaterThan),  ParamArray([App(Call(Dice(Reroll(is,D6))),noValue); Value(Int(i))])) ->  [sprintf "%d+ rerolling (%s)"  (i+1) (String.concat "," (List.map string is)) |> str]
    | App(Call(GreaterThan),  ParamArray([App(Call(Dice(Reroll(is,D3))),noValue); Value(Int(i))])) ->  [sprintf "%d+ rerolling (%s)"  (i+1) (String.concat "," (List.map string is)) |> str]
    | App(Call(GreaterThan),  ParamArray([App(Call(Dice(Reroll(is,Reroll(is2,d)))),noValue); Value(Int(i))])) ->  
        [unparse (App(Call(GreaterThan),  ParamArray([App(Call(Dice(Reroll(List.distinct (is @ is2),d))),noValue); Value(Int(i))]))) |> div []]
    | App(Lam(p,x),a) -> unparse x //paren (unparse (Lam(p,x))) + " " + argstring a
    | App(f,(Var(v))) -> unparse f @ [div [] [sprintf "%s" v |> str]]
    | App(f,a) -> unparse f @ [div [] (unparse a)]
    | Let(v, value, inner) ->  unparse inner


let alternateRoot model dispatch =
    let rec displayOperation operation = 
        match operation with 
        | ParamArray ops  when List.distinct ops = [GameActions.Primitives.State.``D#`` D3] -> str ""
        | ParamArray ops  when List.distinct ops = [GameActions.Primitives.State.``D#`` D6] -> str ""
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

