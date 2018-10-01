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
    | App of f : Operation * value : Operation
    | Lam of param : string * body : Operation
    | Let of string * value : Operation * body : Operation
    | IfThenElse of ifExpr : Operation * thenExpr : Operation * elseExpr : Operation option
    | Choice of name : string * choices : (string * Operation) list

and Call =
    | Product
    | Division
    | Total
    | Count
    | Repeat
    | Dice
    | GreaterThan
    | Contains
    | Equals
    | NotEquals
    | LessThan
    | ToDist
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
let rec (|IsDistribution|_|) =
    function 
    | Value(Dist(d)) | Let(_, IsDistribution(d), _) -> Some d
    | _ -> None

let (|IntCheck|_|) =
    function 
    | Int(i) | Check(Check.Pass(Int(i))) -> Check.Pass i |> Some
    | Check(Check.Fail(Int(i))) -> Check.Fail i |> Some
    | Check(Check.Pass(Float(f))) -> Check.Pass(int f) |> Some
    | Check(Check.Fail(Float(f))) -> Check.Fail(int f) |> Some
    | Float(f) -> Check.Pass(int f) |> Some
    | Str(_) -> None
    | Check(_) -> None
    | NoValue -> Check.Fail 0 |> Some
    | Dist(_) -> None
    | ParamArray(_) -> None
    | Tuple(_) -> None

type GamePrimitive with
    static member Zero = NoValue
    
    static member (-) (x, y) =
        match (x, y) with
        | NoValue, Dist z | Dist z, NoValue -> Dist z
        | NoValue, z | z, NoValue -> z
        | Int(a), Int(b) -> Int(a - b)
        | Float(a), Float(b) -> Float(a - b)
        | Int(b), Float(a) | Float(a), Int(b) -> Float(a - float b)
        | Str(a), Str(b) -> a.Replace(b, "") |> Str
        | Dist d, Dist d2 -> Distribution.combine [ d; d2 ] |> Dist
        | Dist d, gp | gp, Dist d -> Distribution.map ((-) gp) d |> Dist
        | Tuple(Check(Check.Pass(l)), Check(Check.Fail(r))), 
          Check(Check.Pass(r1)) | Check(Check.Pass(r1)), 
                                  Tuple(Check(Check.Pass(l)), 
                                        Check(Check.Fail(r))) -> 
            Tuple(Check(Check.Pass(l - r1)), Check(Check.Fail(r)))
        | Tuple(Check(Check.Pass(l)), Check(Check.Fail(r))), 
          Check(Check.Fail(r2)) | Check(Check.Fail(r2)), 
                                  Tuple(Check(Check.Pass(l)), 
                                        Check(Check.Fail(r))) -> 
            Tuple(Check(Check.Pass(l)), Check(Check.Fail(r2 - r)))
        | Check(Check.Fail(r1)), Check(Check.Fail(r2)) -> 
            Check(Check.Fail(r1 - r2))
        | Check(Check.Pass(r1)), Check(Check.Pass(r2)) -> 
            Check(Check.Pass(r1 - r2))
        | Check(Check.Pass(r1)), Check(Check.Fail(_) as r2) | Check(Check.Fail(_) as r2), 
                                                              Check(Check.Pass(r1)) | r1, 
                                                                                      Check(Check.Fail(_) as r2) | Check(Check.Fail(_) as r2), 
                                                                                                                   r1 -> 
            Tuple(Check(Check.Pass(r1)), Check r2)
        | a, Check(b) | Check(b), a -> 
            Check.combineFavourPass (-) (Check.Pass a) b |> Check
        | Tuple(a, b), Tuple(x, y) -> Tuple(a - x, b - y)
        | Tuple(a, b), x | x, Tuple(a, b) -> Tuple(a - x, b - x)
        | ParamArray a, ParamArray b -> List.append a b |> ParamArray
        | x, y -> 
            failwith <| sprintf "Cannot add these two primitives %A, %A" x y
    
    static member (+) (x, y) =
        match (x, y) with
        | NoValue, Dist z | Dist z, NoValue -> Dist z
        | NoValue, z | z, NoValue -> z
        | Int(a), Int(b) -> Int(a + b)
        | Float(a), Float(b) -> Float(a + b)
        | Int(b), Float(a) | Float(a), Int(b) -> Float(a + float b)
        | Str(a), Str(b) -> Str(a + b)
        | Dist d, Dist d2 -> Distribution.combine [ d; d2 ] |> Dist
        | Dist d, gp | gp, Dist d -> Distribution.map ((+) gp) d |> Dist
        | Tuple(Check(Check.Pass(l)), Check(Check.Fail(r))), 
          Check(Check.Pass(r1)) | Check(Check.Pass(r1)), 
                                  Tuple(Check(Check.Pass(l)), 
                                        Check(Check.Fail(r))) -> 
            Tuple(Check(Check.Pass(l + r1)), Check(Check.Fail(r)))
        | Tuple(Check(Check.Pass(l)), Check(Check.Fail(r))), 
          Check(Check.Fail(r2)) | Check(Check.Fail(r2)), 
                                  Tuple(Check(Check.Pass(l)), 
                                        Check(Check.Fail(r))) -> 
            Tuple(Check(Check.Pass(l)), Check(Check.Fail(r2 + r)))
        | Check(Check.Fail(r1)), Check(Check.Fail(r2)) -> 
            Check(Check.Fail(r1 + r2))
        | Check(Check.Pass(r1)), Check(Check.Pass(r2)) -> 
            Check(Check.Pass(r1 + r2))
        | Check(Check.Pass(r1)), Check(Check.Fail(_) as r2) | Check(Check.Fail(_) as r2), 
                                                              Check(Check.Pass(r1)) | r1, 
                                                                                      Check(Check.Fail(_) as r2) | Check(Check.Fail(_) as r2), 
                                                                                                                   r1 -> 
            Tuple(Check(Check.Pass(r1)), Check r2)
        | a, Check(b) | Check(b), a -> 
            Check.combineFavourPass (+) (Check.Pass a) b |> Check
        | Tuple(a, b), Tuple(x, y) -> Tuple(a + x, b + y)
        | Tuple(a, b), x | x, Tuple(a, b) -> Tuple(a + x, b + x)
        | ParamArray a, ParamArray b -> List.append a b |> ParamArray
        | x, y -> 
            failwith <| sprintf "Cannot add these two primitives %A, %A" x y
    
    static member (*) (x, y) =
        match (x, y) with
        | NoValue, _ | _, NoValue -> NoValue
        | Int(a), Int(b) -> Int(a * b)
        | Float(a), Float(b) -> Float(a * b)
        | Int(b), Float(a) | Float(a), Int(b) -> Float(a * float b)
        | Tuple(a, b), Tuple(x, y) -> Tuple(a * x, b * y)
        | ParamArray ops, ParamArray ops2 when List.length ops = List.length 
                                                                     ops2 -> 
            List.zip ops ops2
            |> List.map (function 
                   | (Value a, Value b) -> a * b |> Value
                   | _ -> Value NoValue)
            |> ParamArray
        | ParamArray [], _ | _, ParamArray [] -> NoValue
        | ParamArray ops, b | b, ParamArray ops -> 
            ops
            |> List.map (fun a -> 
                   match a with
                   | Value a -> a * b |> Value
                   | _ -> Value NoValue)
            |> ParamArray
        | Check r1, Check r2 -> Check.combineFavourFail (*) r1 r2 |> Check
        | a, Check(b) | Check(b), a -> 
            Check.combineFavourFail (*) (Check.Pass a) b |> Check
        | Dist d, Dist d2 -> Distribution.combine [ d; d2 ] |> Dist
        | Dist d, gp | gp, Dist d -> Distribution.map ((*) gp) d |> Dist
        | Str _, _ | _, Str _ -> NoValue
        | Tuple(a, b), x | x, Tuple(a, b) -> Tuple(a * x, b * x)
    
    static member (/) (x, y) =
        match (x, y) with
        | NoValue, _ -> NoValue
        | _, NoValue -> NoValue
        | _, Int(0) | _, Float(0.0) -> NoValue
        | Int(a), Int(b) -> Int(a / b)
        | Float(a), Float(b) -> Float(a / b)
        | Int(a), Float(b) -> Float(float a / b)
        | Float(a), Int(b) -> Float(a / float b)
        | Tuple(a, b), Tuple(x, y) -> Tuple(a / x, b / y)
        | ParamArray ops, ParamArray ops2 when List.length ops = List.length 
                                                                     ops2 -> 
            List.zip ops ops2
            |> List.map (function 
                   | (Value a, Value b) -> a / b |> Value
                   | _ -> Value NoValue)
            |> ParamArray
        | ParamArray [], _ | _, ParamArray [] -> NoValue
        | ParamArray ops, b | b, ParamArray ops -> 
            ops
            |> List.map (fun a -> 
                   match a with
                   | Value a -> a / b |> Value
                   | _ -> Value NoValue)
            |> ParamArray
        | Check r1, Check r2 -> Check.combineFavourFail (/) r1 r2 |> Check
        | a, Check(b) -> Check.combineFavourFail (/) (Check.Pass a) b |> Check
        | Check(a), b -> Check.combineFavourFail (/) a (Check.Pass b) |> Check
        | Dist d, Dist d2 -> Distribution.combine [ d; d2 ] |> Dist
        | Dist d, gp | gp, Dist d -> Distribution.map ((/) gp) d |> Dist
        | Str _, _ | _, Str _ -> NoValue
        | Tuple(a, b), x | x, Tuple(a, b) -> Tuple(a / x, b / x)

type NormalizedOperation =
    | Normal
    | Next of Operation



type Msg = Unit
