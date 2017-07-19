module Result
//type Result< ^a when ^a: (static member Zero : ^a)> = Pass of  ^a | Fail of  ^a | List of Result<  ^a> list | Tuple of  ^a *  ^a 
type Result<'a> = Pass of 'a | Fail of 'a | List of Result< 'a> list | Tuple of 'a * 'a 

let inline add x y = 
    let rec add x y = 
        match x,y with  
        | Pass a, Pass b -> Pass (a + b)
        | Pass a, Fail _ | Fail _, Pass a  -> Pass (a)
        | Fail a, Fail b -> Fail (a + b)
        | List a, List b -> List.map2 add a b |> List
        | a, List bs | List bs, a -> List.map (fun b -> add a b) bs |> List
        | Pass a, Tuple(a',c) | Tuple(a',c),Pass a -> Tuple(a' + a,c + a)
        | Fail a, Tuple(a',c) | Tuple(a',c),Fail a -> Tuple(a',c)
        | Tuple(a,c),Tuple(b,d) -> Tuple(a+b,c+d)
    add x y
let inline mult x y =
    let rec mult x y =
        match x,y with  
        | Pass a, Pass b -> Pass (a * b)
        | Pass _, Fail b | Fail b, Pass _  -> Fail b
        | Fail a, Fail b -> Fail (a * b)
        | List a, List b -> 
            [for a' in a do 
                for b' in b do
                    yield List [a';b']] |> List
        | a, List bs | List bs, a -> List.map (fun b -> mult a b) bs |> List
        | Pass a, Tuple(a',c') | Tuple(a',c'),Pass a -> Tuple(a * a', a * c')
        | Fail c, Tuple(a',c') | Tuple(a',c'),Fail c -> Tuple(c * a', c * c')
        | Tuple(a,c),Tuple(b,d) -> Tuple(a * b,c * d)
    mult x y   
// type Result< ^a when ^a: (static member Zero : ^a)> with
//     static member inline(+) (x,y) = add x y
//     static member inline(*) (x,y) = mult x y
//     static member inline Zero : Result< ^a> = Fail <| LanguagePrimitives.GenericZero   
    // <'T 
let inline map f x = 
    let rec map f x = 
        match x with 
            | Pass a -> Pass (f a) 
            | Fail a -> Fail (f a) 
            | List a -> List.map (map f) a |> List  
            | Tuple(a,b) -> Tuple(f a,f b)
    map f x       
         
let rec printResultF = 
        function
        | Pass x   -> sprintf "Pass %.2f" x 
        | Fail x  -> sprintf "Fail %.2f" x
        | List xs -> List.map printResultF xs |> String.concat ";" |> sprintf "[%s]"
        | Tuple(x,y) -> sprintf "Passes: %.2f, Fails: %.2f" x y
let rec printResultD = 
        function
        | Pass x   -> sprintf "Pass %d" x 
        | Fail x  -> sprintf "Fail %d" x
        | List xs -> List.map printResultD xs |> String.concat ";" |> sprintf "[%s]"
        | Tuple(x,y) -> sprintf "Passes: %d,Fails: %d" x y
let resultListAverage list =
    let divide x y = map (fun x' -> x' / y) x
    match list with 
    | [] -> Fail 0.
    | xs ->
        let mutable sum = Fail 0.
        let mutable count = 0
        for x in xs do
            sum <- add x sum
            count <- count + 1
        divide sum (float count)
