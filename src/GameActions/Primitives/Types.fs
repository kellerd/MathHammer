module GameActions.Primitives.Types

type GamePrimitive =
    | Int of int
    | Str of string
    | Float of float 
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
    | Choice of name : string * choices:(string * Operation) list
and Call = 
    | Product
    | Division
    | Total
    | Count
    | Repeat 
    | Dice 
    | GreaterThan
    | Equals
    | NotEquals
    | LessThan
    | And
    | Or
    | Max
    | Min
    | Sub
    | Median 
    | Mean
    | Mode 
    | Least  
    | Largest 
    //| Reroll of (int list) * Die

let rec (|IsDistribution|_|) = function
    | Value(Dist(d)) | Let(_,IsDistribution(d),_) -> Some d
    | _ -> None
let (|IntCheck|_|) = function 
    | Int(i) 
    | Check(Check.Pass(Int(i))) -> Check.Pass i |> Some 
    | Check(Check.Fail(Int(i))) -> Check.Fail i |> Some
    | Check(Check.Pass(Float(f))) -> Check.Pass (int f) |> Some 
    | Check(Check.Fail(Float(f))) -> Check.Fail (int f) |> Some
    | Float(f) -> Check.Pass (int f) |> Some
    | Str(_) -> None
    | Check(_) -> None
    | NoValue -> Check.Fail 0 |> Some
    | Dist(_) -> None
    | ParamArray(_) -> None
    | Tuple(_) ->None

type GamePrimitive with 
    static member Zero = NoValue
    static member (-) (x,y) = 
        match (x,y) with 
        | NoValue,Dist z | Dist z,NoValue -> Dist z
        | NoValue,z | z,NoValue -> z
        | Int(a),Int(b) -> Int(a-b)
        | Float(a),Float(b) -> Float(a-b)
        | Int(b),Float(a)
        | Float(a),Int(b) -> Float(a-float b)
        | Str(a),Str(b) -> a.Replace(b, "") |> Str
        | Dist d, Dist d2 -> Distribution.combine [d;d2] |> Dist
        | Dist d, gp 
        | gp, Dist d -> Distribution.map ((-) gp) d  |> Dist
        | Tuple (Check (Check.Pass(l)), Check (Check.Fail(r))), Check (Check.Pass(r1))
        | Check (Check.Pass(r1)), Tuple(Check (Check.Pass(l)),Check (Check.Fail(r))) -> Tuple(Check (Check.Pass(l - r1)),Check (Check.Fail(r)))
        | Tuple (Check (Check.Pass(l)),Check (Check.Fail(r))), Check (Check.Fail(r2))
        | Check (Check.Fail(r2)), Tuple(Check (Check.Pass(l)),Check (Check.Fail(r))) -> Tuple(Check (Check.Pass(l)),Check (Check.Fail(r2 - r)))
        | Check (Check.Fail(r1)), Check (Check.Fail(r2)) -> Check(Check.Fail(r1 - r2))
        | Check (Check.Pass(r1)), Check (Check.Pass(r2)) -> Check(Check.Pass(r1 - r2))
        | Check (Check.Pass(r1)), Check (Check.Fail(_) as r2)
        | Check (Check.Fail(_) as r2), Check (Check.Pass(r1))
        | r1, Check (Check.Fail(_) as r2)
        | Check (Check.Fail(_) as r2), r1 -> Tuple(Check (Check.Pass(r1)),Check r2)
        | a, Check(b)
        | Check(b), a -> Check.combineFavourPass (-) (Check.Pass a) b  |> Check
        | Tuple(a,b), Tuple(x,y) -> Tuple(a-x,b-y)
        | Tuple (a,b), x 
        | x, Tuple (a,b) -> Tuple(a-x,b-x)
        | ParamArray a, ParamArray b -> List.append a b |> ParamArray
        | x,y -> failwith <| sprintf "Cannot add these two primitives %A, %A" x y
    static member (+) (x,y) = 
        match (x,y) with 
        | NoValue,Dist z | Dist z,NoValue -> Dist z
        | NoValue,z | z,NoValue -> z
        | Int(a),Int(b) -> Int(a+b)
        | Float(a),Float(b) -> Float(a+b)
        | Int(b),Float(a)
        | Float(a),Int(b) -> Float(a+float b)
        | Str(a),Str(b) -> Str(a+b)
        | Dist d, Dist d2 -> Distribution.combine [d;d2] |> Dist
        | Dist d, gp 
        | gp, Dist d -> Distribution.map ((+) gp) d  |> Dist
        | Tuple (Check (Check.Pass(l)), Check (Check.Fail(r))), Check (Check.Pass(r1))
        | Check (Check.Pass(r1)), Tuple(Check (Check.Pass(l)),Check (Check.Fail(r))) -> Tuple(Check (Check.Pass(l + r1)),Check (Check.Fail(r)))
        | Tuple (Check (Check.Pass(l)),Check (Check.Fail(r))), Check (Check.Fail(r2))
        | Check (Check.Fail(r2)), Tuple(Check (Check.Pass(l)),Check (Check.Fail(r))) -> Tuple(Check (Check.Pass(l)),Check (Check.Fail(r2 + r)))
        | Check (Check.Fail(r1)), Check (Check.Fail(r2)) -> Check(Check.Fail(r1 + r2))
        | Check (Check.Pass(r1)), Check (Check.Pass(r2)) -> Check(Check.Pass(r1 + r2))
        | Check (Check.Pass(r1)), Check (Check.Fail(_) as r2)
        | Check (Check.Fail(_) as r2), Check (Check.Pass(r1))
        | r1, Check (Check.Fail(_) as r2)
        | Check (Check.Fail(_) as r2), r1 -> Tuple(Check (Check.Pass(r1)),Check r2)
        | a, Check(b)
        | Check(b), a -> Check.combineFavourPass (+) (Check.Pass a) b  |> Check
        | Tuple(a,b), Tuple(x,y) -> Tuple(a+x,b+y)
        | Tuple (a,b), x 
        | x, Tuple (a,b) -> Tuple(a+x,b+x)
        | ParamArray a, ParamArray b -> List.append a b |> ParamArray
        | x,y -> failwith <| sprintf "Cannot add these two primitives %A, %A" x y
    static member (*) (x,y) = 
        match (x,y) with 
        | NoValue,_ | _,NoValue -> NoValue
        | Int(a),Int(b) -> Int(a*b)
        | Float(a),Float(b) -> Float(a*b)
        | Int(b),Float(a)
        | Float(a),Int(b) -> Float(a*float b)
        | Tuple(a,b), Tuple(x,y) -> Tuple(a*x,b*y)
        | ParamArray ops, ParamArray ops2 when List.length ops = List.length ops2 ->
            List.zip ops ops2 |> List.map (function (Value a,Value b) -> a * b |> Value | _ -> Value NoValue) |> ParamArray
        | ParamArray [], _ | _, ParamArray [] -> NoValue 
        | ParamArray ops, b | b,  ParamArray ops ->
            ops 
            |> List.map (fun a -> match a with Value a -> a * b |> Value | _ -> Value NoValue) |> ParamArray
        | Check r1, Check r2 -> Check.combineFavourFail (*) r1 r2 |> Check
        | a, Check(b)
        | Check(b), a -> Check.combineFavourFail (*) (Check.Pass a) b  |> Check
        | Dist d, Dist d2 -> Distribution.combine [d;d2] |> Dist
        | Dist d, gp 
        | gp, Dist d -> Distribution.map ((*) gp) d |> Dist
        | Str _, _ | _, Str _ -> NoValue
        | Tuple (a,b), x 
        | x, Tuple (a,b) -> Tuple(a*x,b*x)

    static member (/) (x,y) = 
        match (x,y) with 
        | NoValue,_ -> NoValue
        | _,NoValue -> NoValue
        | _,Int(0) 
        | _,Float(0.0)  -> NoValue
        | Int(a),Int(b) -> Int(a/b)
        | Float(a),Float(b) -> Float(a/b)
        | Int(a),Float(b)   -> Float(float a/b)
        | Float(a),Int(b) -> Float(a/float b)
        | Tuple(a,b), Tuple(x,y) -> Tuple(a/x,b/y)
        | ParamArray ops, ParamArray ops2 when List.length ops = List.length ops2 ->
            List.zip ops ops2 |> List.map (function (Value a,Value b) -> a / b |> Value | _ -> Value NoValue) |> ParamArray
        | ParamArray [], _ | _, ParamArray [] -> NoValue 
        | ParamArray ops, b | b,  ParamArray ops ->
            ops 
            |> List.map (fun a -> match a with Value a -> a / b |> Value | _ -> Value NoValue) |> ParamArray
        | Check r1, Check r2 -> Check.combineFavourFail (/) r1 r2 |> Check
        | a, Check(b) -> Check.combineFavourFail (/) (Check.Pass a) b  |> Check
        | Check(a), b -> Check.combineFavourFail (/)  a (Check.Pass b)  |> Check
        | Dist d, Dist d2 -> Distribution.combine [d;d2] |> Dist
        | Dist d, gp 
        | gp, Dist d -> Distribution.map ((/) gp) d |> Dist
        | Str _, _ | _, Str _ -> NoValue
        | Tuple (a,b), x 
        | x, Tuple (a,b) -> Tuple(a/x,b/x)


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
        | Float(i), Float(i2) -> (if i > i2 then  Float(i) |> Check.Pass  else Float(i) |> Check.Fail) |> Check
        | Float(i), Int(i2) -> (if i > float i2 then  Float(i) |> Check.Pass  else Float(i) |> Check.Fail) |> Check
        | Int(i), Float(i2) -> (if float i > i2 then  Int(i) |> Check.Pass  else Int(i) |> Check.Fail) |> Check
        | Int(i),  Int(i2)  -> (if i > i2 then  Int(i) |> Check.Pass  else Int(i) |> Check.Fail) |> Check
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
        | Str(s),Str(s2) -> (if s < s2 then  Str(s) |> Check.Pass  else Str(s) |> Check.Fail) |> Check  
        | Float(i), Float(i2) -> (if i < i2 then  Float(i) |> Check.Pass  else Float(i) |> Check.Fail) |> Check
        | Float(i), Int(i2) -> (if i < float i2 then  Float(i) |> Check.Pass  else Float(i) |> Check.Fail) |> Check
        | Int(i), Float(i2) -> (if float i < i2 then  Int(i) |> Check.Pass  else Int(i) |> Check.Fail) |> Check  
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
        | Str(s),Str(s2) -> (if s = s2 then  Str(s) |> Check.Pass  else Str(s) |> Check.Fail) |> Check
        | Float(i), Float(i2) -> (if i = i2 then  Float(i) |> Check.Pass  else Float(i) |> Check.Fail) |> Check
        | Float(i), Int(i2) -> (if i = float i2 then  Float(i) |> Check.Pass  else Float(i) |> Check.Fail) |> Check
        | Int(i), Float(i2) -> (if float i = i2 then  Int(i) |> Check.Pass  else Int(i) |> Check.Fail) |> Check  
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
        | Str(s),Str(s2) -> (if s <>s2 then  Str(s) |> Check.Pass  else Str(s) |> Check.Fail) |> Check
        | Float(i), Float(i2) -> (if i <> i2 then  Float(i) |> Check.Pass  else Float(i) |> Check.Fail) |> Check
        | Float(i), Int(i2) -> (if i <> float i2 then  Float(i) |> Check.Pass  else Float(i) |> Check.Fail) |> Check
        | Int(i), Float(i2) -> (if float i <> i2 then  Int(i) |> Check.Pass  else Int(i) |> Check.Fail) |> Check 
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
let rec maxGp gp gp2=  
    GamePrimitive.map2 (fun gp gp2 -> 
        match gp,gp2 with 
        | Int(i),  Int(i2)    -> max i i2 |> Int
        | Float(i),Float(i2)  -> max i i2 |> Float
        | Float(f),Int(i)     -> max (float i) f |> Float
        | Int(i), Float(f)    -> max (float i) f |> Float
        | Str(s),Str(s2)      -> max s s2 |> Str
        | Tuple (ta,ta2), (Tuple (tb,tb2)) -> 
            if ta = tb then
                if (maxGp ta2 tb2) = ta2 then Tuple(ta,ta2)
                else Tuple(tb,tb2)
            else 
                if maxGp ta tb = ta then Tuple(ta,ta2)
                else Tuple(tb,tb2)
        | ParamArray ops, ParamArray ops2 when List.forall (function Value(_) -> true | _ -> false) ops && List.forall (function Value(_) -> true | _ -> false) ops2 ->
            let zipped = List.zip ops ops2 
            List.unfold (function 
                         | (Value gp, Value _ )::tail, Some 1 -> Some(Value gp, (tail,Some 1))
                         | (Value _ ,Value gp2)::tail, Some 2 -> Some(Value gp2,(tail,Some 2))
                         | (Value gp,Value gp2)::tail, None   -> 
                            if gp = gp2 then Some(Value gp,(tail,None))
                            else if maxGp gp gp2 = gp then Some(Value gp,(tail,Some 1)) 
                            else Some(Value gp2,(tail,Some 2))
                         | _ -> None ) (zipped,None)
            |> ParamArray                         
        | gp,gp2 when gp = gp2 -> gp
        | a, NoValue _ | NoValue _, a -> a 
        | Tuple _, _ | _, Tuple _ 
        | Str _, _ | _, Str _ 
        | Check _, _ | _, Check _
        | Dist _, _ | _, Dist _
        | ParamArray _, _ | _, ParamArray _ -> NoValue 
    ) gp gp2

let rec minGp gp gp2=  
    GamePrimitive.map2 (fun gp gp2 -> 
        match gp,gp2 with 
        | Int(i),  Int(i2)    -> min i i2 |> Int
        | Str(s),  Str(s2)    -> min s s2 |> Str
        | Float(i),Float(i2)  -> min i i2 |> Float
        | Float(f),Int(i)     -> min (float i) f |> Float
        | Int(i),  Float(f)   -> min (float i) f |> Float
        | Tuple (ta,ta2), (Tuple (tb,tb2)) -> 
            if ta = tb then
                if (minGp ta2 tb2) = ta2 then Tuple(ta,ta2)
                else Tuple(tb,tb2)
            else 
                if minGp ta tb = ta then Tuple(ta,ta2)
                else Tuple(tb,tb2)
        | ParamArray ops, ParamArray ops2 when List.forall (function Value(_) -> true | _ -> false) ops && List.forall (function Value(_) -> true | _ -> false) ops2 ->
            let zipped = List.zip ops ops2 
            List.unfold (function 
                         | (Value gp, Value _ )::tail, Some 1 -> Some(Value gp, (tail,Some 1))
                         | (Value _ ,Value gp2)::tail, Some 2 -> Some(Value gp2,(tail,Some 2))
                         | (Value gp,Value gp2)::tail, None   -> 
                            if gp = gp2 then Some(Value gp,(tail,None))
                            else if minGp gp gp2 = gp then Some(Value gp,(tail,Some 1)) 
                            else Some(Value gp2,(tail,Some 2))
                         | _ -> None ) (zipped,None)
            |> ParamArray                         
        | gp,gp2 when gp = gp2 -> gp
        | _, NoValue _ | NoValue _, _
        | Tuple _, _ | _, Tuple _ 
        | Str _, _ | _, Str _ 
        | Check _, _ | _, Check _
        | Dist _, _ | _, Dist _
        | ParamArray _, _ | _, ParamArray _ -> NoValue 
    ) gp gp2    

let insert x lst =
    let rec insertCont x cont = function
        | [] -> cont ([x])
        | h::t as l -> 
            match greaterThan x h with 
            | Check (Check.IsFail _) -> cont(x::l)
            | Check (Check.IsPass _) -> insertCont x (fun accLst -> cont(h::accLst)) t
            | _ -> failwith "Can't find correct order"
    insertCont x id lst

// Sorting via insertion
let insertionSort l =
  let rec insertionSortAcc acc = function
    | [] -> acc
    | h::t -> insertionSortAcc (insert h acc) t
  insertionSortAcc [] l
let meanGp values = 
    let count = List.length values |> float
    let sum = List.sum values
    sum * (Float (1. / count))

let meanOp ops = 
    if List.forall (function Value(_) -> true | _ -> false) ops then
        let values = List.map (function | Value(gp) -> gp | _ -> NoValue) ops
        meanGp values |> Value
    else NoValue |> Value
let medianGp ops = 
    if List.forall (function Value(_) -> true | _ -> false) ops then
        let values = List.map (function | Value(gp) -> gp | _ -> NoValue) ops
        let med l = 
            let len = List.length ops 
            let middleValues = if len % 2 = 1 then List.skip (len / 2) l |> List.take 1 else List.skip (len / 2 - 1) l |> List.take 2 
            meanGp middleValues
        insertionSort values |> med |> Value 
    else NoValue |> Value
let modeGp (ops:Operation list) = 
    match List.countBy id ops |> List.sortByDescending snd with 
    | [] -> Value NoValue 
    | [h,_] -> h 
    | (h,c)::tail -> List.filter (fun(_,c') -> c' = c) ((h,c)::tail) |> List.map fst |> ParamArray |> Value
    |> function 
    | Value (ParamArray [x]) -> x
    | x -> x
let leastOp n ops = 
    if List.forall (function Value(_) -> true | _ -> false) ops then
        let values = List.map (function | Value(gp) -> gp | _ -> NoValue) ops
        insertionSort values |> Seq.truncate n |> Seq.map Value |> Seq.toList |> ParamArray |> Value
    else NoValue |> Value    

let largestOp n ops = 
    if List.forall (function Value(_) -> true | _ -> false) ops then
        let values = List.map (function | Value(gp) -> gp | _ -> NoValue) ops
        insertionSort values |> List.rev |> Seq.truncate n |> Seq.map Value |> Seq.rev |> Seq.toList |> ParamArray |> Value
    else NoValue |> Value    

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
                          
type Msg = Unit  

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
            | Float(_)                          -> Scalar "Float"
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