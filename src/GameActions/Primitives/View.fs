module GameActions.Primitives.View

open Fable.Helpers.React
open Props
open Types
open State
open Probability.View

let paren react = str "(" :: react @ [str ")"]

let rec unparseCheck unparseV = function 
    | Check.Pass(v) -> span [Style [Color (colour 255.) ]] <| (str "Pass: ")::(unparseV v)
    | Check.Fail(v) -> span [Style [Color (colour 0.) ]]   <| (str "Fail: ")::(unparseV v)
    // | Check.List(vs) -> div [] [
    //                             yield str "["
    //                             for result in vs do
    //                                 yield unparseCheck unparseV result
    //                                 yield str ";" 
    //                             yield str "]"]
     
let unparseCall func = 
    match func with 
    | Dice(i) -> "D" + string i  |> str
    | Count -> sprintf "(Passes,Fails) in " |> str
    | _  -> sprintf "%A" func |> str  
let unparseDist unparseValue (dist:Distribution.Distribution<GamePrimitive>) = 
    let result = 
          dist.Probabilities 
          |> List.groupBy fst
          |> List.map(fun (f,probs) -> f, List.sumBy snd probs)
    match result with 
    | [] -> str ""
    | _ ->  let max = result |> List.maxBy snd |> snd
            let min = result |> List.minBy snd |> snd
            let total = result |> List.sumBy snd 
            
            result
            |> List.map (fun (r, prob) -> 
                  let colourValue = 
                      match r with 
                      | Check(Check.Pass _ ) -> colourA (float 0xFF) |> Some
                      | Check(Check.Fail _ ) -> colourA (float 0x00) |> Some
                      | Tuple (x,y) when x = y ->  colourA (float 0xFF)  |> Some
                      //| Tuple (Float x,Float y) -> (x / (x + y) ) * float 0xFF |> colourA |> Some
                      | Tuple (Int x,Int y) -> (float x / (float x + float y) ) * float 0xFF  |> colourA|> Some
                      | Int _ -> colourDefault |> Some
                      | _ -> None
                  let colour = 
                    colourValue
                    |> Option.map(fun (colourValue) -> 
                          let alpha = opacity min max prob
                          Style [Color (colourValue alpha)] :> IHTMLProp) 
                    |> Option.toList 
                  match unparseValue r with 
                  | [] -> None 
                  | result -> result @ [str <| sprintf " %.1f%%" (prob / total * 100.)]
                              |> div colour
                              |> Some
                  |> opt)                              
            |> data []    
let rec displayParamArray unparseValue ops =
    match ops with
    | ops when List.distinct ops = [State.``D#`` D6] -> sprintf "%dD6" (List.length ops) |> str
    | ops when List.distinct ops = [State.``D#`` D3] -> sprintf "%dD3" (List.length ops) |> str
    | ops -> section [ClassName "columns"] (List.choose (fun op -> 
        match unparse unparseValue op with 
        | [] -> None 
        | xs -> div [ClassName "column"] xs |> Some) ops)
    //| ops -> List.map unparse ops |> List.reduce (fun l1 l2 -> l1 @ (str " + " :: l2)) |> div []
and unparse unparseValue operation : Fable.Import.React.ReactElement list = 
    match operation with 
    | Call f -> [unparseCall f]
    | Value(v)-> unparseValue v
    | Var (v) -> [sprintf "%s" v |> str]
    | Lam(_) -> []
    | IsDPlus(D6,plus) ->  [string (plus) + "+" |> str]
    | IsDPlus(D3,plus) ->  [string (plus) + "+ on D3" |> str]
    // | App(Call(GreaterThan),  Value(ParamArray([App(Call(Dice(Reroll(is,D6))),_); Value(Int(i))]))) ->  [sprintf "%d+ rerolling (%s)"  (i+1) (String.concat "," (List.map string is)) |> str]
    // | App(Call(GreaterThan),  Value(ParamArray([App(Call(Dice(Reroll(is,D3))),_); Value(Int(i))]))) ->  [sprintf "%d+ rerolling (%s)"  (i+1) (String.concat "," (List.map string is)) |> str]
    // | App(Call(GreaterThan),  Value(ParamArray([App(Call(Dice(Reroll(is,Reroll(is2,d)))),_); Value(Int(i))]))) ->  
    //     [unparse (App(Call(GreaterThan),  Value(ParamArray([App(Call(Dice(Reroll(List.distinct (is @ is2),d))),Value NoValue); Value(Int(i))])))) |> div []]
    | App(Lam(_,x),_) -> unparse unparseValue x //paren (unparse (Lam(p,x))) + " " + argstring a
    | App(f,(Var(v))) -> unparse unparseValue f @ [div [] [sprintf "%s" v |> str]]
    | App(f,a) -> unparse unparseValue f @ [div [] (unparse unparseValue a)]
    | Let(_, _, inner) ->  unparse unparseValue inner
    | PropertyGet(s,op) -> unparse unparseValue op @ [str <| sprintf ".%s" s]
    | IfThenElse(ifExpr, thenExpr, elseExpr) -> 
        let ifPart = str "if " :: unparse unparseValue ifExpr
        let thenPart = str " then " :: br [] :: unparse unparseValue thenExpr
        let elsePart = Option.map(fun elseExpr -> br [] :: unparse unparseValue elseExpr) elseExpr |> Option.toList |> List.collect id
        ifPart @ thenPart @ elsePart

let unparseValue = 
    let rec unparseV = function   
    | Int(i) -> [string i |> str]
    //| Float(f) -> sprintf "%.1f" f |> str
    | Dist(d) -> [unparseDist unparseV d]
    | NoValue -> [str "--" ] 
    | Str s -> [b  [] [str s]]
    | Tuple(v,v2) -> paren <| unparseV v@(str ",")::unparseV v2
    | Check c -> [unparseCheck unparseV c]
    | ParamArray ([]) ->  []
    | ParamArray ([Value (Str _); Value(NoValue)]) ->  []
    | ParamArray ([Value (Str _); Var _]) ->   []
    | ParamArray(m) -> [displayParamArray unparseV m]
    unparseV

let unparseAverage = 
    let rec unparseV = function   
    | Int(i) -> [string i |> str]
    //| Float(f) -> sprintf "%.1f" f |> str
    | Dist(d) -> [unparseDist unparseV d]
    | NoValue -> [str "--" ] 
    | Str s -> [b  [] [str s]]
    | Tuple(v,v2) -> paren <| unparseV v@(str ",")::unparseV v2
    | Check c -> [unparseCheck unparseV c]
    | ParamArray ([]) ->  []
    | ParamArray ([Value (Str _); Value(NoValue)]) ->  []
    | ParamArray ([Value (Str _); Var _]) ->   []
    | ParamArray(m) -> [displayParamArray unparseV m]
    unparseV
let unparseSample = 
    let rec unparseV = function   
    | Int(i) -> [string i |> str]
    //| Float(f) -> sprintf "%.1f" f |> str
    | Dist(d) -> Distribution.sample d |> unparseV
    | NoValue -> [str "--" ] 
    | Str s -> [b  [] [str s]]
    | Tuple(v,v2) -> paren <| unparseV v@(str ",")::unparseV v2
    | Check c -> [unparseCheck unparseV c]
    | ParamArray ([]) ->  []
    | ParamArray ([Value (Str _); Value(NoValue)]) ->  []
    | ParamArray ([Value (Str _); Var _]) ->   []
    | ParamArray(m) -> [displayParamArray unparseV m]
    unparseV

let alternateRoot model _ =
    let rec displayOperation operation = 
        match operation with 
        | Value(ParamArray ops)  when List.distinct ops = [State.``D#`` D3] -> str ""
        | Value(ParamArray ops)  when List.distinct ops = [State.``D#`` D6] -> str ""
        | Call Product -> str ""
        | Call Total  -> str ""
        | Call Repeat  -> str ""
        | Call (Dice _)  -> str ""
        | Call Count -> str ""
        | Call GreaterThan -> str ""
        | Call LessThan -> str ""
        | Call Equals -> str ""
        | Call NotEquals -> str ""
        | Call Or -> str ""
        | Call And -> str ""
        | Value(Int(_)) -> str ""
        | Value(NoValue) -> span [Style [BorderStyle "dotted"; MinWidth 50;MinHeight 50]] []
        | Value(_) ->     str ""
        | Var(_) ->    str ""
        | Let(_) -> str ""
        | App(_) -> str ""
        | Lam(_) -> str ""
        | PropertyGet _ -> str ""
        | IfThenElse(_)  -> str ""
    displayOperation model  

let probabilities model _ = unparse unparseValue model
let averages model _ = unparse unparseValue model
let sample model _ = unparse unparseSample model

