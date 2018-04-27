module GameActions.Primitives.View

open Fable.Helpers.React
open Props
open Types
open State
open Probability.View

let paren react = [str "("; react ; str ")"] |> ofList
let squareParen react = [str "["; react ; str "]"] |> ofList
let rec unparseCheck unparseV = function 
    | Check.Pass(v) -> span [Style [Color (colour 255.) ]] [ str "Pass: "; unparseV v]
    | Check.Fail(v) -> span [Style [Color (colour 0.) ]]   [ str "Fail: "; unparseV v]
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
                  unparseValue r 
                  |> Option.ofObj
                  |> Option.map (fun result -> [result; str <| sprintf " %.1f%%" (prob / total * 100.)] |> div colour)
                  |> ofOption)                     
            |> data []    
let rec displayParamArray unparseValue ops =
    ops
    |> List.choose (unparse unparseValue 
                    >> Option.ofObj
                    >> Option.map (List.singleton >> div [ClassName "column"]))
    |> section [ClassName "columns"]
and unparse unparseValue operation = 
    match operation with 
    | Call f -> str (f.ToString())
    | Value(v)-> unparseValue v
    | Var (v) -> str v
    | Lam(_) -> ofOption None
    | IsDPlus(6,plus) ->  string (plus) + "+" |> str
    | IsDPlus(n,plus) ->  string (plus) + "+ on D" + (string n) |> str
    | Choice(name, _) -> section [ClassName "columns"] [div [ClassName "column" ] [b [] [str ("Choose a " + name)]]]
    // | App(Call(GreaterThan),  Value(ParamArray([App(Call(Dice(Reroll(is,D6))),_); Value(Int(i))]))) ->  [sprintf "%d+ rerolling (%s)"  (i+1) (String.concat "," (List.map string is)) |> str]
    // | App(Call(GreaterThan),  Value(ParamArray([App(Call(Dice(Reroll(is,D3))),_); Value(Int(i))]))) ->  [sprintf "%d+ rerolling (%s)"  (i+1) (String.concat "," (List.map string is)) |> str]
    // | App(Call(GreaterThan),  Value(ParamArray([App(Call(Dice(Reroll(is,Reroll(is2,d)))),_); Value(Int(i))]))) ->  
    //     [unparse (App(Call(GreaterThan),  Value(ParamArray([App(Call(Dice(Reroll(List.distinct (is @ is2),d))),Value NoValue); Value(Int(i))])))) |> div []]
    | App(Call Dice, Value(Int n)) -> string n |> str  
    | App(Call Count, x) -> [str "(Passes,Fails) in "; unparse unparseValue x] |> ofList
    | App(Lam(_,x),_) -> unparse unparseValue x //paren (unparse (Lam(p,x))) + " " + argstring a
    | App(f,(Var(v))) -> [unparse unparseValue f ; str v] |> ofList
    | App(f,a) -> [unparse unparseValue f ; unparse unparseValue a]  |> ofList
    | Let(_, _, inner) ->  unparse unparseValue inner
    | PropertyGet(s,op) -> [unparse unparseValue op ; str ("." + s)]  |> ofList
    | IfThenElse(ifExpr, thenExpr, elseExpr) -> 
        let ifPart = [str "if "; unparse unparseValue ifExpr]   |> ofList
        let thenPart = [str " then "; br []; unparse unparseValue thenExpr]  |> ofList
        let elsePart = Option.map(fun elseExpr -> [br []; unparse unparseValue elseExpr]) elseExpr |> Option.toList |> List.collect id
        ifPart::thenPart::elsePart |> ofList

let unparseValue= 
    let rec unparseV = function
        | Int(i) -> string i |> str
        | Float(f) -> sprintf "%.1f" f |> str
        | Dist(d) -> unparseDist unparseV d
        | NoValue -> str "--"
        | Str s -> b  [] [str s]
        | Tuple(v,v2) -> [unparseV v; (str ","); unparseV v2] |> ofList |> paren  
        | Check c -> unparseCheck unparseV c
        | ParamArray [] ->  ofOption None
        | ParamArray [Value (Str _); Value(NoValue)] ->  ofOption None
        | ParamArray [Value (Str _); Var _] ->   ofOption None
        | ParamArray [Value (Str _); PropertyGet(_,Value(NoValue))] ->   ofOption None
        | ParamArray [Value (Str _); PropertyGet(_,Var _)] ->   ofOption None
        | ParamArray(m) -> displayParamArray unparseV m
    unparseV 

let unparseAverage = 
    let rec unparseV = function   
    | Int(i) -> string i |> str
    | Float(f) -> sprintf "%.7f" f |> str
    | Dist(d) -> 
        d.Probabilities
        |> List.sumBy (fun (a,p) -> a * (Float p)) 
        |> unparseV
    | NoValue -> str "--" 
    | Str s -> b  [] [str s]
    | Tuple(v,v2) -> [unparseV v;(str ",");unparseV v2] |> ofList |> paren 
    | Check c -> unparseCheck unparseV c
    | ParamArray [] -> ofOption None
    | ParamArray [Value (Str _); Value(NoValue)] -> ofOption None
    | ParamArray [Value (Str _); Var _] -> ofOption None
    | ParamArray [Value (Str _); PropertyGet(_,Value(NoValue))]-> ofOption None
    | ParamArray [Value (Str _); PropertyGet(_,Var _)] -> ofOption None
    | ParamArray(m) -> displayParamArray unparseV m
    unparseV

let unparseSample = 
    let rec unparseV = function   
    | Int(i) -> string i |> str
    | Float(f) -> sprintf "%.1f" f |> str
    | Dist(d) -> Distribution.sample d |> unparseV
    | NoValue -> str "--" 
    | Str s -> b [] [str s]
    | Tuple(v,v2) -> [unparseV v; (str ",");unparseV v2] |> ofList |> paren 
    | Check c -> unparseCheck unparseV c
    | ParamArray [] ->  ofOption None
    | ParamArray [Value (Str _); Value(NoValue)] ->  ofOption None
    | ParamArray [Value (Str _); Var _] ->   ofOption None
    | ParamArray [Value (Str _); PropertyGet(_,Value(NoValue))] ->   ofOption None
    | ParamArray [Value (Str _); PropertyGet(_,Var _)] ->   ofOption None
    | ParamArray(m) -> displayParamArray unparseV m
    unparseV

let alternateRoot model _ =
    let rec displayOperation operation = 
        match operation with 
        | Value(ParamArray ops)  when List.distinct ops = [State.``D#`` 3] -> str ""
        | Value(ParamArray ops)  when List.distinct ops = [State.``D#`` 6] -> str ""
        | Value(Int(_)) -> str ""
        | Value(NoValue) -> span [Style [BorderStyle "dotted"; MinWidth 50;MinHeight 50]] []
        | Value(_) ->     str ""
        | Var(_) ->    str ""
        | Let(_) -> str ""
        | App(_) -> str ""
        | Lam(_) -> str ""
        | PropertyGet _ -> str ""
        | IfThenElse(_)  -> str ""
        | Call(_) -> str ""      
        | Choice _ -> str ""
    displayOperation model  

let probabilities model _ = unparse unparseValue model
let averages model _ = unparse unparseAverage model
let sample model _ = unparse unparseSample model

