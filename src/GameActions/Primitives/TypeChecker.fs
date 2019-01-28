module GameActions.Primitives.TypeChecker
open Types
let (|WithLams|_|) =
    let defaultApps apps lams =
        List.append (apps
                     |> List.rev
                     |> List.map Some) 
            (List.init (List.length lams - List.length apps) (fun _ -> None)), 
        lams
    
    let rec (|WithLams'|_|) =
        function 
        | Lam(x, WithLams'(apps, lams, o)) -> Some(apps, x :: lams, o)
        | Lam(x, o) -> Some([], [ x ], o)
        | _ -> None
    
    let rec (|WithApps'|_|) =
        function 
        | App(WithApps'(apps, lams, o), v) when List.length apps < List.length 
                                                                       lams -> 
            Some(v :: apps, lams, o)
        | WithLams'(apps, lams, o) -> Some(apps, lams, o)
        | _ -> None
    
    (|WithApps'|_|) 
    >> Option.map (fun (apps, lams, o) -> (defaultApps apps lams, o))

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

let rec (|IsEmpty|_|) =
    function 
    | Empty -> Some Empty
    | List(IsEmpty(e)) | Distr(IsEmpty(e)) | Pass(IsEmpty(e)) | Pair(IsEmpty(e), 
                                                                     _) | Pair(_, 
                                                                               IsEmpty(e)) | Fail(IsEmpty(e)) -> 
        Some e
    | _ -> None

let toTyped op =
    let rec doCheck =
        function 
        | Int _ -> Scalar "Int"
        | Str(_) -> Scalar "Str"
        | Float(_) -> Scalar "Float"
        | Check(Check.Fail(gp)) -> Fail(doCheck gp)
        | Check(Check.Pass(gp)) -> Pass(doCheck gp)
        | NoValue -> Unknown
        | ParamArray(ops) -> 
            ops
            |> List.fold (fun acc elem -> 
                   match elem with
                   | Value elem -> 
                       match acc, doCheck elem with
                       | IsEmpty(_), a -> a
                       | a, IsEmpty(_) -> a
                       | (a, b) when a <> b -> Mixed
                       | (a, _) -> a
                   | _ -> Unknown) Empty
            |> List
        | Tuple(s, t) -> Pair((doCheck s), (doCheck t))
        | Dist(vs) -> 
            vs.Probabilities
            |> List.fold (fun acc (elem, _) -> 
                   match acc, doCheck elem with
                   | IsEmpty(_), a -> a
                   | a, IsEmpty(_) -> a
                   | (a, b) when a <> b -> Mixed
                   | (a, _) -> a) Empty
            |> Distr
    match op with
    | Value(gp) -> doCheck gp
    | _ -> Unknown

let rec toString =
    function 
    | Fail a -> sprintf "Check<%s>" <| toString a
    | Pass a -> sprintf "Check<%s>" <| toString a
    | Scalar s -> sprintf "Scalar<%s>" s
    | List gpt -> sprintf "List<%s>" <| toString gpt
    | Distr gpt -> sprintf "Dist<%s>" <| toString gpt
    | Mixed -> "Mixed"
    | Unknown -> "Unknown"
    | Empty -> "Empty"
    | Pair(s, t) -> sprintf "Pair<%s,%s>" (toString s) (toString t)

let (|IsPair|IsCheck|IsList|IsDistr|IsScalar|IsOther|) operation =
    match toTyped operation with
    | Pair(gp, gp2) -> IsPair(gp, gp2)
    | List gp -> IsList(gp)
    | Pass gp -> IsCheck(gp)
    | Fail gp -> IsCheck(gp)
    | Distr gp -> IsDistr(gp)
    | Empty -> IsOther
    | Mixed -> IsOther
    | Unknown -> IsOther
    | Scalar gp -> IsScalar(gp)
