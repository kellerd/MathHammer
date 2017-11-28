module GameActions.Primitives.Types
open Check    
open Elmish.React.Common
open System.Globalization
type Die =
    | D3
    | D6
    | Reroll of (int list) * Die
type GamePrimitive =
    | Int of int
    | Str of string
    | Float of float
    | Check of Check<GamePrimitive>
    | NoValue 
    | Dist of Distribution.Distribution<GamePrimitive>
and Operation = 
    | Call of Call
    | PropertyGet of string * Operation
    | ParamArray of Operation list
    | Value of GamePrimitive
    | Var of string
    | App of f:Operation * value:Operation
    | Lam of param:string * body:Operation
    | Let of string * value:Operation * body:Operation
    | IfThenElse of ifExpr:Operation * thenExpr:Operation * elseExpr:Operation option
and Call = 
    | Product
    | Total
    | Count
    | Repeat 
    | Dice of Die
    | GreaterThan
    | Equals
    | NotEquals
    | LessThan
    | And
    | Or

// let x = 
//     <@ 
//         let a = 5
//         let b = 10
//         if a > b then "" elif a >= b then "a" else "ab" @>    
let rec (|IsDistribution|_|) = function
    | Value(Dist(d)) | Let(_,IsDistribution(d),_) -> Some d
    | _ -> None
let (|IntCheck|_|) = function 
    | Int(i) 
    | Check(Pass(Int(i))) -> Pass i |> Some 
    | Check(Fail(Int(i))) -> Fail i |> Some
    | Str(_) -> None
    | Check(_) -> None
    | NoValue -> Fail 0 |> Some
    | Dist(_) -> None
    | Float _ -> None

type GamePrimitive with 
    static member Zero = NoValue
    static member (+) (x,y) = 
        match (x,y) with 
        | NoValue,Dist z | Dist z,NoValue -> z |> Distribution.normalize |> Dist 
        | NoValue,z | z,NoValue -> z
        | Int(a),Int(b) -> Int(a+b)
        | Float(a),Float(b) -> Float(a+b)
        | Str(a),Str(b) -> Str(a+b)
        | Dist d, Dist d2 -> Distribution.combine [d;d2] |> Dist
        | Dist d, gp 
        | gp, Dist d -> Distribution.map ((+) gp) d |> Distribution.normalize |> Dist
        | Check (r1),Check(r2) -> Check.add r1 r2 |> Check
        | Float(x), Int(y) 
        | Int(y), Float(x)  -> Float(x + float y)
        | x,y -> failwith <| sprintf "Cannot add these two primitives %A, %A" x y
    static member (*) (x,y) = 
        match (x,y) with 
        | NoValue,z | z,NoValue -> NoValue
        | Int(a),Int(b) -> Int(a*b)
        | Float(a),Float(b) -> Float(a*b)
        | Check r1, Check r2 -> Check.mult r1 r2 |> Check
        | Dist _, Check(List [])
        | Dist _, Check(Fail (_))
        | Check(List []), Dist _ 
        | Check(Fail (_)), Dist _ -> NoValue
        | Check(Tuple(Int(n),_)), Dist d2
        | Dist d2, Check(Tuple(Int(n),_))
        | Check(Pass (Int(n))), Dist d2 
        | Dist d2, Check(Pass (Int(n)))
        | Dist d2, Int (n) 
        | Int (n), Dist d2 -> 
                match List.replicate n d2 with 
                | [] -> [] 
                | head::tail -> tail |> List.fold (fun d d2 -> Distribution.cartesian d d2 |> Distribution.map (fun (a,b) -> a + b)) head
                |> Dist
        | Dist d2, Check(List xs)
        | Check(List xs), Dist d2-> 
            let n = 
                List.fold (fun c elem -> 
                            match elem with 
                            | Pass _ -> c + 1 
                            | Fail _ -> c 
                            | Tuple(Int(x),_) -> c + x 
                            | _ -> c) 0 xs    
            match List.replicate n d2 with 
            | [] -> [] 
            | head::tail -> tail |> List.fold (fun d d2 -> Distribution.cartesian d d2 |> Distribution.map (fun (a,b) -> a + b)) head
            |> Dist                       
        | x,y -> failwith <| sprintf "Cannot multiply these two primitives %A, %A" x y

let rec greaterThan gp gp2 = 
    match gp,gp2 with 
    | Int(i),  Int(i2)  -> (if i > i2 then  Int(i) |> Pass  else Int(i) |> Fail) |> Check
    | Float(i),  Float(i2)  -> (if i > i2 then  Float(i) |> Pass  else Float(i) |> Fail) |> Check
    | Int a, Float b -> (if float a > b then Int(a) |> Pass else Int(a) |> Fail) |> Check
    | Float a, Int b -> (if a > float b then Float(a) |> Pass else Float(a) |> Fail) |> Check
    | Dist(d), Dist(d2) -> List.map2(fun (a,p1) (b,p2) -> greaterThan a b,p1) d d2 |> Dist
    | Check(r), Check(r2) -> Check.bind (fun a -> Check.map(fun b -> greaterThan a b ) r2) r |> Check
    | Str(s),Str(s2) -> (if s > s2 then  Str(s) |> Pass  else Str(s) |> Fail) |> Check
    | gp, Dist(d) -> List.map(fun (b,p1) -> greaterThan gp b,p1) d |> Dist
    | Dist(d),gp -> List.map(fun (a,p1) -> greaterThan a gp,p1) d |> Dist
    | gp,Check(r) -> Check.map(fun b -> greaterThan gp b) r |> Check
    | Check(r), gp -> Check.map(fun a -> greaterThan a gp) r |> Check
    | NoValue,NoValue -> Fail NoValue |> Check
    | _, NoValue _ | NoValue _, _ 
    | Str _, _ | _, Str _ -> 
        //printfn "Couldn't compare %A > %A" gp gp2;
        NoValue
let rec lessThan op op2 = 
    match op,op2 with 
    | Int(i),  Int(i2)  -> (if i < i2 then  Int(i) |> Pass  else Int(i) |> Fail) |> Check
    | Float(i),  Float(i2)  -> (if i < i2 then  Float(i) |> Pass  else Float(i) |> Fail) |> Check
    | Int a, Float b -> (if float a < b then Int(a) |> Pass else Int(a) |> Fail) |> Check
    | Float a, Int b -> (if a < float b then Float(a) |> Pass else Float(a) |> Fail) |> Check
    | Dist(d), Dist(d2) -> List.map2(fun (a,p1) (b,p2) -> lessThan a b,p1) d d2 |> Dist
    | Check(r), Check(r2) -> Check.bind (fun a -> Check.map(fun b -> lessThan a b ) r2) r |> Check
    | Str(s),Str(s2) -> (if s < s2 then  Str(s) |> Pass  else Str(s) |> Fail) |> Check    
    | gp, Dist(d) -> List.map(fun (b,p1) -> lessThan gp b,p1) d |> Dist
    | Dist(d),gp -> List.map(fun (a,p1) -> lessThan a gp,p1) d |> Dist
    | gp,Check(r) -> Check.map(fun b -> lessThan gp b) r |> Check
    | Check(r), gp -> Check.map(fun a -> lessThan a gp) r |> Check
    | NoValue,NoValue -> Fail NoValue |> Check
    | _, NoValue _ | NoValue _, _ 
    | Str _, _ | _, Str _ -> 
        //printfn "Couldn't compare %A < %A" op op2; 
        NoValue
let rec equals op op2 = 
    match op,op2 with
    | Int(i),  Int(i2)  -> (if i = i2 then  Int(i) |> Pass  else Int(i) |> Fail) |> Check
    | Float(i),  Float(i2)  -> (if i = i2 then  Float(i) |> Pass  else Float(i) |> Fail) |> Check
    | Int a, Float b | Float b, Int a -> (if float a = b then Int(a) |> Pass else Int(a) |> Fail) |> Check
    | Dist(d), Dist(d2) -> (if d = d2 then Dist(d) |> Pass else Dist(d) |> Fail) |> Check
    | Check(r), Check(r2) -> Check.bind (fun a -> Check.map(fun b -> equals a b ) r2) r |> Check
    | Str(s),Str(s2) -> (if s = s2 then  Str(s) |> Pass  else Str(s) |> Fail) |> Check
    | gp, Dist(d) -> List.map(fun (b,p1) -> equals gp b,p1) d |> Dist
    | Dist(d),gp -> List.map(fun (a,p1) -> equals a gp,p1) d |> Dist
    | gp,Check(r) -> Check.map(fun b -> equals gp b) r |> Check
    | Check(r), gp -> Check.map(fun a -> equals a gp) r |> Check
    | NoValue,NoValue -> Pass NoValue |> Check
    | _, NoValue _ | NoValue _, _ 
    | Str _, _ | _, Str _ -> //printfn "Couldn't compare %A = %A" op op2;
        NoValue
let rec notEquals op op2 = 
    match op,op2 with 
    | Int(i),  Int(i2)  -> (if i <> i2 then  Int(i) |> Pass  else Int(i) |> Fail) |> Check
    | Float(i),  Float(i2)  -> (if i <> i2 then  Float(i) |> Pass  else Float(i) |> Fail) |> Check
    | Int a, Float b | Float b, Int a -> (if float a <> b then Int(a) |> Pass else Int(a) |> Fail) |> Check
    | Dist(d), Dist(d2) -> (if d <> d2 then Dist(d) |> Pass else Dist(d) |> Fail) |> Check
    | Check(r), Check(r2) -> Check.bind (fun a -> Check.map(fun b -> notEquals a b ) r2) r |> Check
    | Str(s),Str(s2) -> (if s <>s2 then  Str(s) |> Pass  else Str(s) |> Fail) |> Check
    | gp, Dist(d) -> List.map(fun (b,p1) -> notEquals gp b,p1) d |> Dist
    | Dist(d),gp -> List.map(fun (a,p1) -> notEquals a gp,p1) d |> Dist
    | gp,Check(r) -> Check.map(fun b -> notEquals gp b) r |> Check
    | Check(r), gp -> Check.map(fun a -> notEquals a gp) r |> Check
    | gp,gp2 when gp = gp2 -> Fail gp |> Check
    | _, NoValue _ | NoValue _, _ 
    | Str _, _ | _, Str _ -> // printfn "Couldn't compare %A <> %A" op op2; 
        NoValue
        
type NormalizedOperation = Normal | Next of Operation    

type [<Measure>] ft 
and [<Measure>] inch
and [<Measure>] mm

type ft with    
    static member ToInch(a:int<ft>) : int<inch> = a * 12<inch/ft>
    static member ToMM(a:int<ft>) : int<mm> = a * 305<mm/ft>
    static member FromInch(a:int<inch>) : int<ft> = a / 12<inch/ft>
    static member FromMM(a:int<mm>) : int<ft> = a / 305<mm/ft>
    static member ToInchf(a:float<ft>) : float<inch> = a * 12.<inch/ft>
    static member ToMMf(a:float<ft>) : float<mm> = a * 305.<mm/ft>
    static member FromInchf(a:float<inch>) : float<ft> = a / 12.<inch/ft>
    static member FromMMf(a:float<mm>) : float<ft> = a / 305.<mm/ft>
type inch with     
    static member ToFt : int<inch>->int<ft> = ft.FromInch
    static member ToMM(a:int<inch>) : int<mm> = a * 25<mm/inch>
    static member FromMM(a:int<mm>) : int<inch> = a / 25<mm/inch>
    static member FromFt : int<ft>->int<inch> = ft.ToInch
    static member ToFtf : float<inch>->float<ft> = ft.FromInchf
    static member ToMMf(a:float<inch>) : float<mm> = a * 25.<mm/inch>
    static member FromMMf(a:float<mm>) : float<inch> = a / 25.<mm/inch>
    static member FromFtf : float<ft>->float<inch> = ft.ToInchf
type mm with 
    static member ToInch : int<mm>->int<inch> = inch.FromMM
    static member ToFt : int<mm>->int<ft>= ft.FromMM
    static member FromInch : int<inch>->int<mm>= inch.ToMM
    static member FromFt : int<ft>->int<mm> = ft.ToMM
    static member ToInchf : float<mm>->float<inch> = inch.FromMMf
    static member ToFtf : float<mm>->float<ft>= ft.FromMMf
    static member FromInchf : float<inch>->float<mm>= inch.ToMMf
    static member FromFtf : float<ft>->float<mm> = ft.ToMMf
                          
type Msg =  Unit  


