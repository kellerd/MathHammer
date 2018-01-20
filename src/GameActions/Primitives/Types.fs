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
        | NoValue,Dist z | Dist z,NoValue -> Dist z
        | NoValue,z | z,NoValue -> z
        | Int(a),Int(b) -> Int(a+b)
        //| Float(a),Float(b) -> Float(a+b)
        | Str(a),Str(b) -> Str(a+b)
        | Dist d, Dist d2 -> Distribution.combine [d;d2] |> Dist
        | Dist d, gp 
        | gp, Dist d -> Distribution.map ((+) gp) d  |> Dist
        | Check (Check.Pass(r1)), Tuple(Check (Check.Pass(l)),Check (Check.Fail(r))) -> Tuple(Check (Check.Pass(l + r1)),Check (Check.Fail(r)))
        | Check (Check.Fail(r2)), Tuple(Check (Check.Pass(l)),Check (Check.Fail(r))) -> Tuple(Check (Check.Pass(l)),Check (Check.Fail(r2 + r)))
        | Check (Check.Fail(r1)), Check (Check.Fail(r2)) -> Check(Check.Fail(r1 + r2))
        | Check (Check.Pass(r1)), Check (Check.Pass(r2)) -> Check(Check.Pass(r1 + r2))
        | Check (Check.Pass(r1)), Check (Check.Fail(_) as r2)
        | Check (Check.Fail(_) as r2), Check (Check.Pass(r1))
        | r1, Check (Check.Fail(_) as r2)
        | Check (Check.Fail(_) as r2), r1 -> Tuple(Check (Check.Pass(r1)),Check r2)
        | a, Check(b)
        | Check(b), a -> Check.add (Check.Pass a) b  |> Check
        //| Float(x), Int(y) 
        //| Int(y), Float(x)  -> Float(x + float y)
        | Tuple(a,b), Tuple(x,y) -> Tuple(a+x,b+y)
        | Tuple (a,b), x 
        | x, Tuple (a,b) -> Tuple(a+x,b)
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
        | gp, Dist d -> Distribution.map ((*) gp) d |> Dist
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

module TypeChecker = 
    type GamePrimitiveType = 
    | Pair of GamePrimitiveType * GamePrimitiveType
    | List of GamePrimitiveType 
    | Pass of GamePrimitiveType 
    | Fail of GamePrimitiveType 
    | Distr of GamePrimitiveType 
    | Empty 
    | Mixed 
    | Unknown
    | Scalar of string

    let rec (|IsEmpty|_|) = function 
        | Empty -> Some Empty
        | List(IsEmpty(e)) 
        | Distr(IsEmpty(e)) 
        | Pass(IsEmpty(e)) 
        | Pair(IsEmpty(e), _)
        | Pair(_, IsEmpty(e))
        | Fail(IsEmpty(e)) -> Some e
        | _ -> None 
    let toTyped op =
        let rec doCheck = function
            | Int _                             -> Scalar "Int"
            | Str(_)                            -> Scalar "Str"
            //| Float(_)                          -> Scalar (Primitive "Float")
            | Check(Check.Fail(gp))             -> Fail (doCheck gp)
            | Check(Check.Pass(gp))             -> Pass (doCheck gp)
            | NoValue                           -> Unknown
            | ParamArray(ops)  ->
                ops
                |> List.fold (fun acc elem ->
                    match elem with
                    | Value elem ->
                        match acc,doCheck elem with
                        | IsEmpty(_), a -> a
                        | a, IsEmpty(_) -> a
                        | (a, b) when a <> b -> Mixed
                        | (a,_) -> a
                    | _ -> Unknown ) Empty
                |> List
            | Tuple(s, t) -> 
                Pair ((doCheck s),(doCheck t))
            | Dist(vs) ->
                vs.Probabilities
                |> List.fold (fun acc (elem,_) ->
                    match acc,doCheck elem with
                      | IsEmpty(_), a -> a
                      | a, IsEmpty(_) -> a
                      | (a, b) when a <> b -> Mixed
                      | (a,_) -> a ) Empty
                |> Distr
        match op with
        | Value(gp) -> doCheck gp
        | _ -> Unknown
    let rec toString = function
        | Fail a -> sprintf "Check<%s>" <| toString a
        | Pass a -> sprintf "Check<%s>" <| toString a
        | Scalar s -> sprintf "Scalar<%s>" s
        | List gpt -> sprintf "List<%s>" <| toString gpt
        | Distr gpt -> sprintf "Dist<%s>" <| toString gpt
        | Mixed -> "Mixed"
        | Unknown -> "Unknown"
        | Empty -> "Empty"
        | Pair (s,t) -> sprintf "Pair<%s,%s>" (toString s) (toString t)
    let (|IsPair|IsCheck|IsList|IsDistr|IsScalar|IsOther|) operation = 
        match toTyped operation with 
        | Pair (gp,gp2) -> IsPair    (gp,gp2) 
        | List   gp     -> IsList    (gp)
        | Pass   gp     -> IsCheck   (gp)
        | Fail   gp     -> IsCheck   (gp)
        | Distr  gp     -> IsDistr   (gp)
        | Empty         -> IsOther    
        | Mixed         -> IsOther    
        | Unknown       -> IsOther    
        | Scalar gp     -> IsScalar  (gp)
