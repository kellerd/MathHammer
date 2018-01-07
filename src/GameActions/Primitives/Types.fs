module GameActions.Primitives.Types
type Die =
    | D3
    | D6
    | Reroll of (int list) * Die
type GamePrimitive =
    | Int of int
    | Str of string
    //| Float of float
    | Check of Check.Check<GamePrimitive>
    | NoValue 
    | ParamArray of Operation list
    | Tuple of GamePrimitive * GamePrimitive
    | Dist of Distribution.Distribution<GamePrimitive>
and Operation = 
    | Call of Call
    | PropertyGet of string * Operation
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

let rec (|IsDistribution|_|) = function
    | Value(Dist(d)) | Let(_,IsDistribution(d),_) -> Some d
    | _ -> None
let (|IntCheck|_|) = function 
    | Int(i) 
    | Check(Check.Pass(Int(i))) -> Check.Pass i |> Some 
    | Check(Check.Fail(Int(i))) -> Check.Fail i |> Some
    | Str(_) -> None
    | Check(_) -> None
    | NoValue -> Check.Fail 0 |> Some
    | Dist(_) -> None
    //| Float _ -> None
    | ParamArray(_) -> None
    | Tuple(_) ->None

type GamePrimitive with 
    static member Zero = NoValue
    static member (+) (x,y) = 
        match (x,y) with 
        | NoValue,Dist z | Dist z,NoValue -> z |> Distribution.normalize |> Dist 
        | NoValue,z | z,NoValue -> z
        | Int(a),Int(b) -> Int(a+b)
        //| Float(a),Float(b) -> Float(a+b)
        | Str(a),Str(b) -> Str(a+b)
        | Dist d, Dist d2 -> Distribution.combine [d;d2] |> Dist
        | Dist d, gp 
        | gp, Dist d -> Distribution.map ((+) gp) d |> Distribution.normalize |> Dist
        | Check (r1),Check(r2) -> Check.add r1 r2 |> Check
        | a, Check(b)
        | Check(b), a -> Check.add (Check.Pass a) b  |> Check
        //| Float(x), Int(y) 
        //| Int(y), Float(x)  -> Float(x + float y)
        | Tuple(a,b), Tuple(x,y) -> Tuple(a+x,b+y)
        | ParamArray a, ParamArray b -> List.append a b |> ParamArray
        | x,y -> failwith <| sprintf "Cannot add these two primitives %A, %A" x y
    static member (*) (x,y) = 
        match (x,y) with 
        | NoValue,_ | _,NoValue -> NoValue
        | Int(a),Int(b) -> Int(a*b)
        //| Float(a),Float(b) -> Float(a*b)
        | Tuple(a,b), Tuple(x,y) -> Tuple(a*x,b*y)
        | ParamArray ops, ParamArray ops2 when List.length ops = List.length ops2 ->
            List.zip ops ops2 |> List.map (function (Value a,Value b) -> a * b |> Value | _ -> Value NoValue) |> ParamArray
        | ParamArray [], _ | _, ParamArray [] -> NoValue 
        | ParamArray ops, b | b,  ParamArray ops ->
            ops 
            |> List.map (fun a -> match a with Value a -> a * b |> Value | _ -> Value NoValue) |> ParamArray
        | Check r1, Check r2 -> Check.mult r1 r2 |> Check
        | a, Check(b)
        | Check(b), a -> Check.mult (Check.Pass a) b  |> Check
        | Dist d, Dist d2 -> Distribution.combine [d;d2] |> Dist
        | Dist d, gp 
        | gp, Dist d -> Distribution.map ((*) gp) d |> Distribution.normalize |> Dist
        | Str _, _ | _, Str _ -> NoValue
        | Tuple _,_ | _,Tuple _ -> NoValue


module GamePrimitive =
    let map2 f x y =
        match x,y with  
        | Dist(r), Dist(r2) -> Distribution.bind (fun a -> Distribution.map(fun b -> f a b ) r2) r |> Dist
        | Check(r), Check(r2) -> Check.bind (fun a -> Check.map(fun b -> f a b ) r2) r |> Check
        | gp,Dist(r) -> Distribution.map(f gp) r |> Dist
        | Dist(r), gp -> Distribution.map(fun a -> f a gp) r |> Dist
        | gp,Check(r) -> Check.map(fun b -> f gp b) r |> Check
        | Check(r), gp -> Check.map(fun a -> f a gp) r |> Check
        | x,y -> f x y

let greaterThan = 
    GamePrimitive.map2 (fun gp gp2 -> 
        match gp,gp2 with 
        | Int(i),  Int(i2)  -> (if i > i2 then  Int(i) |> Check.Pass  else Int(i) |> Check.Fail) |> Check
        //| Float(i),  Float(i2)  -> (if i > i2 then  Float(i) |> Check.Pass  else Float(i) |> Check.Fail) |> Check
        //| Int a, Float b -> (if float a > b then Int(a) |> Check.Pass else Int(a) |> Check.Fail) |> Check
        //| Float a, Int b -> (if a > float b then Float(a) |> Check.Pass else Float(a) |> Check.Fail) |> Check
        | Str(s),Str(s2) -> (if s > s2 then  Str(s) |> Check.Pass  else Str(s) |> Check.Fail) |> Check
        | NoValue,NoValue -> Check.Fail NoValue |> Check
        | (Tuple _ as t), (Tuple _ as t2) -> (if t > t2 then  t |> Check.Pass  else t |> Check.Fail) |> Check
        | Tuple _, _ | _, Tuple _ 
        | _, NoValue _ | NoValue _, _ 
        | Str _, _ | _, Str _ 
        | Check _, _ | _, Check _
        | Dist _, _ | _, Dist _
        | ParamArray _, _ | _, ParamArray _ -> NoValue //printfn "Couldn't compare %A > %A" gp gp2;
    )
let lessThan = 
    GamePrimitive.map2 (fun gp gp2 -> 
        match gp,gp2 with 
        | Int(i),  Int(i2)  -> (if i < i2 then  Int(i) |> Check.Pass  else Int(i) |> Check.Fail) |> Check
        //| Float(i),  Float(i2)  -> (if i < i2 then  Float(i) |> Check.Pass  else Float(i) |> Check.Fail) |> Check
        //| Int a, Float b -> (if float a < b then Int(a) |> Check.Pass else Int(a) |> Check.Fail) |> Check
        //| Float a, Int b -> (if a < float b then Float(a) |> Check.Pass else Float(a) |> Check.Fail) |> Check
        | Str(s),Str(s2) -> (if s < s2 then  Str(s) |> Check.Pass  else Str(s) |> Check.Fail) |> Check    
        | NoValue,NoValue -> Check.Fail NoValue |> Check
        | (Tuple _ as t), (Tuple _ as t2) -> (if t < t2 then  t |> Check.Pass  else t |> Check.Fail) |> Check
        | Tuple _, _ | _, Tuple _ 
        | _, NoValue _ | NoValue _, _ 
        | Str _, _ | _, Str _ 
        | Check _, _ | _, Check _
        | Dist _, _ | _, Dist _
        | ParamArray _, _ | _, ParamArray _ -> NoValue //printfn "Couldn't compare %A > %A" gp gp2;
    )
let equals = 
    GamePrimitive.map2 (fun gp gp2 -> 
        match gp,gp2 with 
        | Int(i),  Int(i2)  -> (if i = i2 then  Int(i) |> Check.Pass  else Int(i) |> Check.Fail) |> Check
        // | Float(i),  Float(i2)  -> (if i = i2 then  Float(i) |> Check.Pass  else Float(i) |> Check.Fail) |> Check
        // | Int a, Float b ->  (if float a = b then Int(a) |> Check.Pass else Int(a) |> Check.Fail) |> Check
        // | Float a, Int b -> (if float b = a then Float(a) |> Check.Pass else Float(a) |> Check.Fail) |> Check
        | Str(s),Str(s2) -> (if s = s2 then  Str(s) |> Check.Pass  else Str(s) |> Check.Fail) |> Check
        | NoValue,NoValue -> Check.Pass NoValue |> Check
        | ParamArray ops, ParamArray ops2 ->  (if ops = ops2 then ParamArray ops |> Check.Pass  else ParamArray ops |> Check.Fail) |> Check
        | (Tuple _ as t), (Tuple _ as t2) -> (if t = t2 then  t |> Check.Pass  else t |> Check.Fail) |> Check
        | Tuple _, _ | _, Tuple _ 
        | _, NoValue _ | NoValue _, _ 
        | Str _, _ | _, Str _ 
        | Check _, _ | _, Check _
        | Dist _, _ | _, Dist _
        | ParamArray _, _ | _, ParamArray _ -> NoValue //printfn "Couldn't compare %A > %A" gp gp2;
        )
let rec notEquals = 
    GamePrimitive.map2 (fun gp gp2 -> 
        match gp,gp2 with 
        | Int(i),  Int(i2)  -> (if i <> i2 then  Int(i) |> Check.Pass  else Int(i) |> Check.Fail) |> Check
        // | Float(i),  Float(i2)  -> (if i <> i2 then  Float(i) |> Check.Pass  else Float(i) |> Check.Fail) |> Check
        // | Int a, Float b | Float b, Int a -> (if float a <> b then Int(a) |> Check.Pass else Int(a) |> Check.Fail) |> Check
        | Str(s),Str(s2) -> (if s <>s2 then  Str(s) |> Check.Pass  else Str(s) |> Check.Fail) |> Check
        | (Tuple _ as t), (Tuple _ as t2) -> (if t <> t2 then  t |> Check.Pass  else t |> Check.Fail) |> Check
        | ParamArray ops, ParamArray ops2 ->  (if ops <> ops2 then ParamArray ops |> Check.Pass  else ParamArray ops |> Check.Fail) |> Check
        | gp,gp2 when gp = gp2 -> Check.Fail gp |> Check
        | Tuple _, _ | _, Tuple _ 
        | _, NoValue _ | NoValue _, _ 
        | Str _, _ | _, Str _ 
        | Check _, _ | _, Check _
        | Dist _, _ | _, Dist _
        | ParamArray _, _ | _, ParamArray _ -> NoValue //printfn "Couldn't compare %A > %A" gp gp2;
    )

let checkGp f = 
    let (|AsCheck|) v = 
        match v with
        | NoValue -> Check.Fail NoValue
        | Check c -> c
        | v -> Check.Pass v 
    let rec checkGp' gp gp2 = 
        match gp,gp2 with 
        | Dist(a), Dist(b) -> 
            Distribution.combine [a;b]
            |> Distribution.groupBy (function AsCheck(Check.Pass a) -> a | AsCheck(Check.Fail b) -> b) checkGp'
            |> Dist
        | Dist(a), b       ->    Distribution.map (fun a     -> checkGp' a b) a |> Dist
        | a, Dist(b)       ->    Distribution.map (fun b     -> checkGp' a b) b |> Dist
        | AsCheck a, AsCheck b -> 
            f a b |> Check
    checkGp'
let rec andGp = checkGp Check.all
let rec orGp = checkGp Check.either

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
    static member ToFt a = a
    static member ToMM(a:int<inch>) : int<mm> = a * 25<mm/inch>
    static member FromMM(a:int<mm>) : int<inch> = a / 25<mm/inch>
    static member FromFt a = ft.ToInch a
    static member ToFtf a = ft.FromInchf a
    static member ToMMf(a:float<inch>) : float<mm> = a * 25.<mm/inch>
    static member FromMMf(a:float<mm>) : float<inch> = a / 25.<mm/inch>
    static member FromFtf a = ft.ToInchf a
type mm with 
    static member ToInch a = inch.FromMM a
    static member ToFt a = ft.FromMM a
    static member FromInch a = inch.ToMM a
    static member FromFt a = ft.ToMM a
    static member ToInchf a = inch.FromMMf a
    static member ToFtf a = ft.FromMMf a
    static member FromInchf a = inch.ToMMf a
    static member FromFtf a = ft.ToMMf a
                          
type Msg =  Unit  


