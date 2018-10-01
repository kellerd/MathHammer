
module GameActions.Primitives.GamePrimitiveOperations 
open GameActions.Primitives.Types
let map2 f x y =
    match x, y with
    | Dist(r), Dist(r2) -> 
        Distribution.bind (fun a -> Distribution.map (fun b -> f a b) r2) r 
        |> Dist
    | Check(r), Check(r2) -> 
        Check.bind (fun a -> Check.map (fun b -> f a b) r2) r |> Check
    | gp, Dist(r) -> Distribution.map (f gp) r |> Dist
    | Dist(r), gp -> Distribution.map (fun a -> f a gp) r |> Dist
    | gp, Check(r) -> Check.map (fun b -> f gp b) r |> Check
    | Check(r), gp -> Check.map (fun a -> f a gp) r |> Check
    | x, y -> f x y
let contains =
    map2 
        (fun gp gp2 -> 
        match gp, gp2 with
        ParamArray gs, (Int _ | Float(_) | Tuple _ | NoValue) -> 
            if gs |> List.contains (Value gp2) then ParamArray gs |> Check.Pass
            else ParamArray gs |> Check.Fail 
            |> Check
        | Str(s)  , Str(s2) -> 
            (if s.Contains(s2) then Str(s) |> Check.Pass
             else Str(s) |> Check.Fail)
            |> Check
        | ParamArray gs, Str(s2) -> 
            match List.tryFind(function  Value(Str s) -> s.Contains(s2) | _ -> false ) gs with 
            | Some _ -> ParamArray gs |> Check.Pass
            | None -> ParamArray gs |> Check.Fail 
            |> Check
        | _ -> NoValue //printfn "Couldn't compare %A > %A" gp gp2;
                    )
let greaterThan =
    map2 
        (fun gp gp2 -> 
        match gp, gp2 with
        | Float(i), Float(i2) -> 
            (if i > i2 then Float(i) |> Check.Pass
             else Float(i) |> Check.Fail)
            |> Check
        | Float(i), Int(i2) -> 
            (if i > float i2 then Float(i) |> Check.Pass
             else Float(i) |> Check.Fail)
            |> Check
        | Int(i), Float(i2) -> 
            (if float i > i2 then Int(i) |> Check.Pass
             else Int(i) |> Check.Fail)
            |> Check
        | Int(i), Int(i2) -> 
            (if i > i2 then Int(i) |> Check.Pass
             else Int(i) |> Check.Fail)
            |> Check
        | Str(s), Str(s2) -> 
            (if s > s2 then Str(s) |> Check.Pass
             else Str(s) |> Check.Fail)
            |> Check
        | NoValue, NoValue -> Check.Fail NoValue |> Check
        | (Tuple _ as t), (Tuple _ as t2) -> 
            (if t > t2 then t |> Check.Pass
             else t |> Check.Fail)
            |> Check
        | Tuple _, _ | _, Tuple _ | _, NoValue _ | NoValue _, _ | Str _, _ | _, 
                                                                             Str _ | Check _, 
                                                                                     _ | _, 
                                                                                         Check _ | Dist _, 
                                                                                                   _ | _, 
                                                                                                       Dist _ | ParamArray _, 
                                                                                                                _ | _, 
                                                                                                                    ParamArray _ -> 
            NoValue //printfn "Couldn't compare %A > %A" gp gp2;
                    )

let lessThan =
    map2 
        (fun gp gp2 -> 
        match gp, gp2 with
        | Int(i), Int(i2) -> 
            (if i < i2 then Int(i) |> Check.Pass
             else Int(i) |> Check.Fail)
            |> Check
        | Str(s), Str(s2) -> 
            (if s < s2 then Str(s) |> Check.Pass
             else Str(s) |> Check.Fail)
            |> Check
        | Float(i), Float(i2) -> 
            (if i < i2 then Float(i) |> Check.Pass
             else Float(i) |> Check.Fail)
            |> Check
        | Float(i), Int(i2) -> 
            (if i < float i2 then Float(i) |> Check.Pass
             else Float(i) |> Check.Fail)
            |> Check
        | Int(i), Float(i2) -> 
            (if float i < i2 then Int(i) |> Check.Pass
             else Int(i) |> Check.Fail)
            |> Check
        | NoValue, NoValue -> Check.Fail NoValue |> Check
        | (Tuple _ as t), (Tuple _ as t2) -> 
            (if t < t2 then t |> Check.Pass
             else t |> Check.Fail)
            |> Check
        | Tuple _, _ | _, Tuple _ | _, NoValue _ | NoValue _, _ | Str _, _ | _, 
                                                                             Str _ | Check _, 
                                                                                     _ | _, 
                                                                                         Check _ | Dist _, 
                                                                                                   _ | _, 
                                                                                                       Dist _ | ParamArray _, 
                                                                                                                _ | _, 
                                                                                                                    ParamArray _ -> 
            NoValue //printfn "Couldn't compare %A > %A" gp gp2;
                    )

let equals =
    map2 
        (fun gp gp2 -> 
        match gp, gp2 with
        | Int(i), Int(i2) -> 
            (if i = i2 then Int(i) |> Check.Pass
             else Int(i) |> Check.Fail)
            |> Check
        | Str(s), Str(s2) -> 
            (if s = s2 then Str(s) |> Check.Pass
             else Str(s) |> Check.Fail)
            |> Check
        | Float(i), Float(i2) -> 
            (if i = i2 then Float(i) |> Check.Pass
             else Float(i) |> Check.Fail)
            |> Check
        | Float(i), Int(i2) -> 
            (if i = float i2 then Float(i) |> Check.Pass
             else Float(i) |> Check.Fail)
            |> Check
        | Int(i), Float(i2) -> 
            (if float i = i2 then Int(i) |> Check.Pass
             else Int(i) |> Check.Fail)
            |> Check
        | NoValue, NoValue -> Check.Pass NoValue |> Check
        | ParamArray ops, ParamArray ops2 -> 
            (if ops = ops2 then ParamArray ops |> Check.Pass
             else ParamArray ops |> Check.Fail)
            |> Check
        | (Tuple _ as t), (Tuple _ as t2) -> 
            (if t = t2 then t |> Check.Pass
             else t |> Check.Fail)
            |> Check
        | Tuple _, _ | _, Tuple _ | _, NoValue _ | NoValue _, _ | Str _, _ | _, 
                                                                             Str _ | Check _, 
                                                                                     _ | _, 
                                                                                         Check _ | Dist _, 
                                                                                                   _ | _, 
                                                                                                       Dist _ | ParamArray _, 
                                                                                                                _ | _, 
                                                                                                                    ParamArray _ -> 
            NoValue //printfn "Couldn't compare %A > %A" gp gp2;
                    )

let rec notEquals =
    map2 
        (fun gp gp2 -> 
        match gp, gp2 with
        | Int(i), Int(i2) -> 
            (if i <> i2 then Int(i) |> Check.Pass
             else Int(i) |> Check.Fail)
            |> Check
        | Str(s), Str(s2) -> 
            (if s <> s2 then Str(s) |> Check.Pass
             else Str(s) |> Check.Fail)
            |> Check
        | Float(i), Float(i2) -> 
            (if i <> i2 then Float(i) |> Check.Pass
             else Float(i) |> Check.Fail)
            |> Check
        | Float(i), Int(i2) -> 
            (if i <> float i2 then Float(i) |> Check.Pass
             else Float(i) |> Check.Fail)
            |> Check
        | Int(i), Float(i2) -> 
            (if float i <> i2 then Int(i) |> Check.Pass
             else Int(i) |> Check.Fail)
            |> Check
        | (Tuple _ as t), (Tuple _ as t2) -> 
            (if t <> t2 then t |> Check.Pass
             else t |> Check.Fail)
            |> Check
        | ParamArray ops, ParamArray ops2 -> 
            (if ops <> ops2 then ParamArray ops |> Check.Pass
             else ParamArray ops |> Check.Fail)
            |> Check
        | gp, gp2 when gp = gp2 -> Check.Fail gp |> Check
        | Tuple _, _ | _, Tuple _ | _, NoValue _ | NoValue _, _ | Str _, _ | _, 
                                                                             Str _ | Check _, 
                                                                                     _ | _, 
                                                                                         Check _ | Dist _, 
                                                                                                   _ | _, 
                                                                                                       Dist _ | ParamArray _, 
                                                                                                                _ | _, 
                                                                                                                    ParamArray _ -> 
            NoValue //printfn "Couldn't compare %A > %A" gp gp2;
                    )

let rec maxGp gp gp2 =
    map2 
        (fun gp gp2 -> 
        match gp, gp2 with
        | Int(i), Int(i2) -> max i i2 |> Int
        | Float(i), Float(i2) -> max i i2 |> Float
        | Float(f), Int(i) -> max (float i) f |> Float
        | Int(i), Float(f) -> max (float i) f |> Float
        | Str(s), Str(s2) -> max s s2 |> Str
        | Tuple(ta, ta2), (Tuple(tb, tb2)) -> 
            if ta = tb then 
                if (maxGp ta2 tb2) = ta2 then Tuple(ta, ta2)
                else Tuple(tb, tb2)
            else if maxGp ta tb = ta then Tuple(ta, ta2)
            else Tuple(tb, tb2)
        | ParamArray ops, ParamArray ops2 when List.forall (function 
                                                   | Value(_) -> true
                                                   | _ -> false) ops
                                               && List.forall (function 
                                                      | Value(_) -> true
                                                      | _ -> false) ops2 -> 
            let zipped = List.zip ops ops2
            List.unfold (function 
                | (Value gp, Value _) :: tail, Some 1 -> 
                    Some(Value gp, (tail, Some 1))
                | (Value _, Value gp2) :: tail, Some 2 -> 
                    Some(Value gp2, (tail, Some 2))
                | (Value gp, Value gp2) :: tail, None -> 
                    if gp = gp2 then Some(Value gp, (tail, None))
                    else if maxGp gp gp2 = gp then 
                        Some(Value gp, (tail, Some 1))
                    else Some(Value gp2, (tail, Some 2))
                | _ -> None) (zipped, None)
            |> ParamArray
        | gp, gp2 when gp = gp2 -> gp
        | a, NoValue _ | NoValue _, a -> a
        | Tuple _, _ | _, Tuple _ | Str _, _ | _, Str _ | Check _, _ | _, 
                                                                       Check _ | Dist _, 
                                                                                 _ | _, 
                                                                                     Dist _ | ParamArray _, 
                                                                                              _ | _, 
                                                                                                  ParamArray _ -> 
            NoValue) gp gp2

let rec minGp gp gp2 =
    map2 
        (fun gp gp2 -> 
        match gp, gp2 with
        | Int(i), Int(i2) -> min i i2 |> Int
        | Str(s), Str(s2) -> min s s2 |> Str
        | Float(i), Float(i2) -> min i i2 |> Float
        | Float(f), Int(i) -> min (float i) f |> Float
        | Int(i), Float(f) -> min (float i) f |> Float
        | Tuple(ta, ta2), (Tuple(tb, tb2)) -> 
            if ta = tb then 
                if (minGp ta2 tb2) = ta2 then Tuple(ta, ta2)
                else Tuple(tb, tb2)
            else if minGp ta tb = ta then Tuple(ta, ta2)
            else Tuple(tb, tb2)
        | ParamArray ops, ParamArray ops2 when List.forall (function 
                                                   | Value(_) -> true
                                                   | _ -> false) ops
                                               && List.forall (function 
                                                      | Value(_) -> true
                                                      | _ -> false) ops2 -> 
            let zipped = List.zip ops ops2
            List.unfold (function 
                | (Value gp, Value _) :: tail, Some 1 -> 
                    Some(Value gp, (tail, Some 1))
                | (Value _, Value gp2) :: tail, Some 2 -> 
                    Some(Value gp2, (tail, Some 2))
                | (Value gp, Value gp2) :: tail, None -> 
                    if gp = gp2 then Some(Value gp, (tail, None))
                    else if minGp gp gp2 = gp then 
                        Some(Value gp, (tail, Some 1))
                    else Some(Value gp2, (tail, Some 2))
                | _ -> None) (zipped, None)
            |> ParamArray
        | gp, gp2 when gp = gp2 -> gp
        | _, NoValue _ | NoValue _, _ | Tuple _, _ | _, Tuple _ | Str _, _ | _, 
                                                                             Str _ | Check _, 
                                                                                     _ | _, 
                                                                                         Check _ | Dist _, 
                                                                                                   _ | _, 
                                                                                                       Dist _ | ParamArray _, 
                                                                                                                _ | _, 
                                                                                                                    ParamArray _ -> 
            NoValue) gp gp2

let insert x lst =
    let rec insertCont x cont =
        function 
        | [] -> cont ([ x ])
        | h :: t as l -> 
            match greaterThan x h with
            | Check(Check.IsFail _) -> cont (x :: l)
            | Check(Check.IsPass _) -> 
                insertCont x (fun accLst -> cont (h :: accLst)) t
            | _ -> failwith "Can't find correct order"
    insertCont x id lst

// Sorting via insertion
let insertionSort l =
    let rec insertionSortAcc acc =
        function 
        | [] -> acc
        | h :: t -> insertionSortAcc (insert h acc) t
    insertionSortAcc [] l

let meanGp values =
    let count = List.length values |> float
    let sum = List.sum values
    sum * (Float(1. / count))

let meanOp ops =
    if List.forall (function 
           | Value(_) -> true
           | _ -> false) ops
    then 
        let values =
            List.map (function 
                | Value(gp) -> gp
                | _ -> NoValue) ops
        meanGp values |> Value
    else NoValue |> Value

let medianGp ops =
    if List.forall (function 
           | Value(_) -> true
           | _ -> false) ops
    then 
        let values =
            List.map (function 
                | Value(gp) -> gp
                | _ -> NoValue) ops
        
        let med l =
            let len = List.length ops
            
            let middleValues =
                if len % 2 = 1 then List.skip (len / 2) l |> List.take 1
                else List.skip (len / 2 - 1) l |> List.take 2
            meanGp middleValues
        
        insertionSort values
        |> med
        |> Value
    else NoValue |> Value

let modeGp (ops : Operation list) =
    match List.countBy id ops |> List.sortByDescending snd with
    | [] -> Value NoValue
    | [ h, _ ] -> h
    | (h, c) :: tail -> 
        List.filter (fun (_, c') -> c' = c) ((h, c) :: tail)
        |> List.map fst
        |> ParamArray
        |> Value
    |> function 
    | Value(ParamArray [ x ]) -> x
    | x -> x

let leastOp n ops =
    if List.forall (function 
           | Value(_) -> true
           | _ -> false) ops
    then 
        let values =
            List.map (function 
                | Value(gp) -> gp
                | _ -> NoValue) ops
        insertionSort values
        |> Seq.truncate n
        |> Seq.map Value
        |> Seq.toList
        |> ParamArray
        |> Value
    else NoValue |> Value

let largestOp n ops =
    if List.forall (function 
           | Value(_) -> true
           | _ -> false) ops
    then 
        let values =
            List.map (function 
                | Value(gp) -> gp
                | _ -> NoValue) ops
        insertionSort values
        |> List.rev
        |> Seq.truncate n
        |> Seq.map Value
        |> Seq.rev
        |> Seq.toList
        |> ParamArray
        |> Value
    else NoValue |> Value

let checkGp f =
    let (|AsCheck|) v =
        match v with
        | NoValue -> Check.Fail NoValue
        | Check c -> c
        | v -> Check.Pass v
    
    let rec checkGp' gp gp2 =
        match gp, gp2 with
        | Dist(a), Dist(b) -> 
            Distribution.combine [ a; b ]
            |> Distribution.groupBy (function 
                   | AsCheck(Check.Pass a) -> a
                   | AsCheck(Check.Fail b) -> b) checkGp'
            |> Dist
        | Dist(a), b -> Distribution.map (fun a -> checkGp' a b) a |> Dist
        | a, Dist(b) -> Distribution.map (fun b -> checkGp' a b) b |> Dist
        | AsCheck a, AsCheck b -> f a b |> Check
    
    checkGp'

let rec andGp = checkGp Check.all

let rec orGp = checkGp Check.either

