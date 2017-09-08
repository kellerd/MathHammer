module GameActions.Primitives.Types
open Result    

type Die =
    | D3
    | D6
    | Reroll of (int list) * Die
type GamePrimitive =
    | Int of int
    | Str of string
    | Result of Result<GamePrimitive>
    | NoValue 
    | Dist of Distribution.Distribution<GamePrimitive>
and Operation = 
    | Call of Call
    | ParamArray of ParamArray
    | Value of GamePrimitive
    | Var of string
    | App of f:Operation * value:Operation
    | Lam of param:string * body:Operation
    | Let of string * value:Operation * body:Operation
and Call = 
    | Product
    | Total
    | Count
    | Unfold 
    | Dice of Die
    | GreaterThan
    | Equals
    | NotEquals
    | LessThan
    | And
    | Or
and ParamArray =
    | OpList of Operation list
    
let rec (|IsDistribution|_|) = function
    | Value(Dist(d)) | Let(_,IsDistribution(d),_) -> Some d
    | _ -> None
let (|IntResult|_|) = function 
    | Int(i) 
    | Result(Pass(Int(i))) -> Pass i |> Some 
    | Result(Fail(Int(i))) -> Fail i |> Some
    | Str(_) -> None
    | Result(_) -> None
    | NoValue -> Fail 0 |> Some
    | Dist(_) -> None

type GamePrimitive with 
    static member Zero = NoValue
    static member (+) (x,y) = 
        match (x,y) with 
        | NoValue,z | z,NoValue -> z
        | Int(a),Int(b) -> Int(a+b)
        | Str(a),Str(b) -> Str(a+b)
        | Dist d, Dist d2 -> Distribution.combine [d;d2] |> Dist
        | Result (r1),Result(r2) -> Result.add r1 r2 |> Result
        | x,y -> failwith <| sprintf "Cannot add these two primitives %A, %A" x y
    static member (*) (x,y) = 
        let rec cartSeq (nss:seq<#seq<'a>>) = 
              let f0 (n:'a) (nss:seq<#seq<'a>>) = 
                match Seq.isEmpty nss with
                | true -> Seq.singleton(Seq.singleton n)
                | _ -> Seq.map (fun (nl:#seq<'a>)->seq{yield n; yield! nl}) nss
              match Seq.isEmpty nss with
              | true -> Seq.empty
              | _ -> Seq.collect (fun n->f0 n (cartSeq (Seq.skip 1 nss))) (Seq.head nss)
        match (x,y) with 
        | NoValue,z | z,NoValue -> NoValue
        | Int(a),Int(b) -> Int(a*b)
        | Str(a),Str(b) -> 
            cartSeq [a.ToCharArray();b.ToCharArray()] 
            |> Seq.map(string)
            |> String.concat "\n"
            |> Str
        | Result r1, Result r2 -> Result.mult r1 r2 |> Result
        //| Dist d, Dist d2 -> Distribution.cartesian d d2 |> Distribution.map (fun (d1,d2) -> ParamArray(OpList[Value(d1);Value(d2)])) |> Dist
        | x,y -> failwith <| sprintf "Cannot multiply these two primitives %A, %A" x y
let rec greaterThan gp gp2 = 
    match gp,gp2 with 
    | Int(i),  Int(i2)  -> (if i > i2 then  Int(i) |> Pass  else Int(i) |> Fail) |> Result
    | Dist(d), Dist(d2) -> List.map2(fun (a,p1) (b,p2) -> greaterThan a b,p1) d d2 |> Dist
    | Result(r), Result(r2) -> Result.bind (fun a -> Result.map(fun b -> greaterThan a b ) r2) r |> Result
    | Str(s),Str(s2) -> (if s > s2 then  Str(s) |> Pass  else Str(s) |> Fail) |> Result
    | gp, Dist(d) -> List.map(fun (b,p1) -> greaterThan gp b,p1) d |> Dist
    | Dist(d),gp -> List.map(fun (a,p1) -> greaterThan a gp,p1) d |> Dist
    | gp,Result(r) -> Result.map(fun b -> greaterThan gp b) r |> Result
    | Result(r), gp -> Result.map(fun a -> greaterThan a gp) r |> Result
    | _, NoValue _ | NoValue _, _ 
    | Str _, _ | _, Str _ -> printf "Couldn't compare %A > %A" gp gp2; NoValue
let rec lessThan op op2 = 
    match op,op2 with 
    | Int(i),  Int(i2)  -> (if i < i2 then  Int(i) |> Pass  else Int(i) |> Fail) |> Result
    | Dist(d), Dist(d2) -> List.map2(fun (a,p1) (b,p2) -> lessThan a b,p1) d d2 |> Dist
    | Result(r), Result(r2) -> Result.bind (fun a -> Result.map(fun b -> lessThan a b ) r2) r |> Result
    | Str(s),Str(s2) -> (if s < s2 then  Str(s) |> Pass  else Str(s) |> Fail) |> Result    
    | gp, Dist(d) -> List.map(fun (b,p1) -> lessThan gp b,p1) d |> Dist
    | Dist(d),gp -> List.map(fun (a,p1) -> lessThan a gp,p1) d |> Dist
    | gp,Result(r) -> Result.map(fun b -> lessThan gp b) r |> Result
    | Result(r), gp -> Result.map(fun a -> lessThan a gp) r |> Result
    | _, NoValue _ | NoValue _, _ 
    | Str _, _ | _, Str _ -> printf "Couldn't compare %A < %A" op op2; NoValue
let rec equals op op2 = 
    match op,op2 with 
    | Int(i),  Int(i2)  -> (if i = i2 then  Int(i) |> Pass  else Int(i) |> Fail) |> Result
    | Dist(d), Dist(d2) -> List.map2(fun (a,p1) (b,p2) -> equals a b,p1) d d2 |> Dist
    | Result(r), Result(r2) -> Result.bind (fun a -> Result.map(fun b -> equals a b ) r2) r |> Result
    | Str(s),Str(s2) -> (if s = s2 then  Str(s) |> Pass  else Str(s) |> Fail) |> Result
    | gp, Dist(d) -> List.map(fun (b,p1) -> equals gp b,p1) d |> Dist
    | Dist(d),gp -> List.map(fun (a,p1) -> equals a gp,p1) d |> Dist
    | gp,Result(r) -> Result.map(fun b -> equals gp b) r |> Result
    | Result(r), gp -> Result.map(fun a -> equals a gp) r |> Result
    | _, NoValue _ | NoValue _, _ 
    | Str _, _ | _, Str _ -> printf "Couldn't compare %A = %A" op op2;NoValue
let rec notEquals op op2 = 
    match op,op2 with 
    | Int(i),  Int(i2)  -> (if i <> i2 then  Int(i) |> Pass  else Int(i) |> Fail) |> Result
    | Dist(d), Dist(d2) -> List.map2(fun (a,p1) (b,p2) -> notEquals a b,p1) d d2 |> Dist
    | Result(r), Result(r2) -> Result.bind (fun a -> Result.map(fun b -> notEquals a b ) r2) r |> Result
    | Str(s),Str(s2) -> (if s <>s2 then  Str(s) |> Pass  else Str(s) |> Fail) |> Result
    | gp, Dist(d) -> List.map(fun (b,p1) -> notEquals gp b,p1) d |> Dist
    | Dist(d),gp -> List.map(fun (a,p1) -> notEquals a gp,p1) d |> Dist
    | gp,Result(r) -> Result.map(fun b -> notEquals gp b) r |> Result
    | Result(r), gp -> Result.map(fun a -> notEquals a gp) r |> Result
    | _, NoValue _ | NoValue _, _ 
    | Str _, _ | _, Str _ -> printf "Couldn't compare %A <> %A" op op2; NoValue
        
        
type NormalizedOperation = Normal | Next of Operation    

type [<Measure>] ft 
and [<Measure>] inch
and [<Measure>] mm

type ft with    
    static member ToInch(a:int<ft>) : int<inch> = a * 12<inch/ft>
    static member ToMM(a:int<ft>) : int<mm> = a * 305<mm/ft>
    static member FromInch(a:int<inch>) : int<ft> = a / 12<inch/ft>
    static member FromMM(a:int<mm>) : int<ft> = a / 305<mm/ft>
type inch with     
    static member ToFt : int<inch>->int<ft> = ft.FromInch
    static member ToMM(a:int<inch>) : int<mm> = a * 25<mm/inch>
    static member FromMM(a:int<mm>) : int<inch> = a / 25<mm/inch>
    static member FromFt : int<ft>->int<inch> = ft.ToInch
type mm with 
    static member ToInch : int<mm>->int<inch> = inch.FromMM
    static member ToFt : int<mm>->int<ft>= ft.FromMM
    static member FromInch : int<inch>->int<mm>= inch.ToMM
    static member FromFt : int<ft>->int<mm> = ft.ToMM

                          
type Msg =  Unit  


