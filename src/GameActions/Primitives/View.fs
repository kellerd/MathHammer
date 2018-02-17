module GameActions.Primitives.View

open Fable.Helpers.React
open Props
open Types
open State
open Probability.View

type DisplayType = 
    | DInt of int
    | DStr of string
    | DFloat of float
    | DCheck of Check.Check<DisplayType>
    | DNoValue 
    | DParamArray of Operation list
    | DTuple of DisplayType * DisplayType
    | DDist of Distribution.Distribution<DisplayType>
let rec toDisplay = function 
    | Int a        -> DInt a       
    | Str a        -> DStr a       
    | Check a      -> Check.map (toDisplay) a |> DCheck
    | NoValue      -> DNoValue     
    | ParamArray a -> DParamArray a
    | Tuple (a,b)  -> DTuple (toDisplay a, toDisplay b)     
    | Dist a       -> Distribution.map (toDisplay) a |> DDist 
let rec toGamePrimitive = function 
    | DInt a        -> Int a       
    | DStr a        -> Str a  
    | DFloat a      -> Int (int a)     
    | DCheck a      -> Check.map (toGamePrimitive) a |> Check
    | DNoValue      -> NoValue     
    | DParamArray a -> ParamArray a
    | DTuple (a,b)  -> Tuple (toGamePrimitive a, toGamePrimitive b)     
    | DDist a       -> Distribution.map (toGamePrimitive) a |> Dist 
type DisplayType with    
    static member Zero = DNoValue
    static member (+) (x,y) = 
            match (x,y) with 
            | DNoValue,DDist z | DDist z,DNoValue -> DDist z
            | DNoValue,z | z,DNoValue -> z
            | DInt(a),DInt(b) -> DInt(a+b)
            | DFloat(a),DFloat(b) -> DFloat(a+b)
            | DStr(a),DStr(b) -> DStr(a+b)
            | DDist d, DDist d2 -> Distribution.combine [d;d2] |> DDist
            | DDist d, gp 
            | gp, DDist d -> Distribution.map ((+) gp) d  |> DDist
            | DTuple (DCheck (Check.Pass(l)),DCheck (Check.Fail(r))), DCheck (Check.Pass(r1))
            | DCheck (Check.Pass(r1)), DTuple (DCheck (Check.Pass(l)),DCheck (Check.Fail(r))) -> DTuple(DCheck (Check.Pass(l + r1)),DCheck (Check.Fail(r)))
            | DTuple (DCheck (Check.Pass(l)),DCheck (Check.Fail(r))), DCheck (Check.Fail(r2))
            | DCheck (Check.Fail(r2)), DTuple (DCheck (Check.Pass(l)),DCheck (Check.Fail(r))) -> DTuple(DCheck (Check.Pass(l)),DCheck (Check.Fail(r2 + r)))
            | DCheck (Check.Fail(r1)), DCheck (Check.Fail(r2)) -> DCheck(Check.Fail(r1 + r2))
            | DCheck (Check.Pass(r1)), DCheck (Check.Pass(r2)) -> DCheck(Check.Pass(r1 + r2))
            | DCheck (Check.Pass(r1)), DCheck (Check.Fail(_) as r2)
            | DCheck (Check.Fail(_) as r2), DCheck (Check.Pass(r1))
            | r1, DCheck (Check.Fail(_) as r2)
            | DCheck (Check.Fail(_) as r2), r1 -> DTuple(DCheck (Check.Pass(r1)),DCheck r2)
            | a, DCheck(b)
            | DCheck(b), a -> Check.add (Check.Pass a) b  |> DCheck
            | DFloat(x), DInt(y) 
            | DInt(y), DFloat(x)  -> DFloat(x + float y)
            | DTuple(a,b), DTuple(x,y) -> DTuple(a+x,b+y)
            | DTuple (a,b), x 
            | x, DTuple (a,b) -> DTuple(a+x,b+x)
            | DParamArray a, DParamArray b -> List.append a b |> DParamArray
            | _ -> DNoValue
    static member (*) (x,y) = 
            match (x,y) with 
            | DNoValue,_ | _,DNoValue -> DNoValue
            | DInt(a),DInt(b) -> DInt(a*b)
            | DFloat(a),DFloat(b) -> DFloat(a*b)
            | DInt(a),DFloat(b) -> DFloat(float a*b)
            | DFloat(a),DInt(b) -> DFloat(a*float b)
            | DTuple(a,b), DTuple(x,y) -> DTuple(a*x,b*y)
            | DParamArray ops, DParamArray ops2 when List.length ops = List.length ops2 ->
                List.zip ops ops2 |> List.map (function (Value a,Value b) -> a * b |> Value | _ -> Value NoValue) |> DParamArray
            | DParamArray [], _ | _, DParamArray [] -> DNoValue 
            | DParamArray ops, b | b,  DParamArray ops ->
                ops 
                |> List.map (fun a -> match a with Value a -> (toDisplay a) * b |> toGamePrimitive |> Value | _ -> Value NoValue) |> DParamArray
            | DCheck r1, DCheck r2 -> Check.mult r1 r2 |> DCheck
            | a, DCheck(b)
            | DCheck(b), a -> Check.mult (Check.Pass a) b  |> DCheck
            | DDist d, DDist d2 -> Distribution.combine [d;d2] |> DDist
            | DDist d, gp 
            | gp, DDist d -> Distribution.map ((*) gp) d |> DDist
            | DStr _, _ | _, DStr _ -> DNoValue
            | DTuple (a,b), x 
            | x, DTuple (a,b) -> DTuple(a*x,b*x)

//DCheck(Check.Fail(DFloat 6.0)) + DCheck(Check.Pass(DFloat 12.0)) + DCheck(Check.Pass(DFloat 6.0)) + DCheck(Check.Pass(DFloat 12.0))         
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
    | Choice(name, _) -> [str <| sprintf "Choose a %s" name]
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
    | DInt(i) -> [string i |> str]
    | DFloat(f) -> [sprintf "%.7f" f |> str]
    | DDist(d) -> 
        d.Probabilities
        |> List.sumBy (fun (a,p) -> a * (DFloat p)) 
        |> unparseV
    | DNoValue -> [str "--" ] 
    | DStr s -> [b  [] [str s]]
    | DTuple(v,v2) -> paren <| unparseV v@(str ",")::unparseV v2
    | DCheck c -> [unparseCheck unparseV c]
    | DParamArray ([]) ->  []
    | DParamArray ([Value (Str _); Value(NoValue)]) ->  []
    | DParamArray ([Value (Str _); Var _]) ->   []
    | DParamArray(m) -> [displayParamArray (toDisplay >> unparseV) m]
    toDisplay >> unparseV

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
        | Choice _ -> str ""
    displayOperation model  

let probabilities model _ = unparse unparseValue model
let averages model _ = unparse unparseAverage model
let sample model _ = unparse unparseSample model

