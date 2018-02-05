[<RequireQualifiedAccess>]
module Check
//type Check< ^a when ^a: (static member Zero : ^a)> = Pass of  ^a | Fail of  ^a | List of Check<  ^a> list | Tuple of  ^a *  ^a 
type Check<'a> = Pass of 'a | Fail of 'a //| List of Check< 'a> list | Tuple of 'a * 'a 
let inline add x y = 
    let rec add x y = 
        match x,y with  
        | Pass a, Pass b -> Pass (a + b)
        | Pass a, Fail _ | Fail _, Pass a  -> Pass (a)
        | Fail a, Fail b -> Fail (a + b)
        // | List a, List b -> List.map2 add a b |> List
        // | a, List bs | List bs, a -> List.map (fun b -> add a b) bs |> List
        // | Pass a, Tuple(a',c) | Tuple(a',c),Pass a -> Tuple(a' + a,c)
        // | Fail a, Tuple(a',c) | Tuple(a',c),Fail a -> Tuple(a',c + a)
        // | Tuple(a,c),Tuple(b,d) -> Tuple(a+b,c+d)
    add x y
let inline mult x y =
    let rec mult x y =
        match x,y with  
        | Pass a, Pass b -> Pass (a * b)
        | Pass a, Fail b | Fail b, Pass a  -> Fail (a * b)
        | Fail a, Fail b -> Fail (a * b)
        // | List a, List b -> 
        //     [for a' in a do 
        //         for b' in b do
        //             yield List [a';b']] |> List
        // | a, List bs | List bs, a -> List.map (fun b -> mult a b) bs |> List
        // | Pass a, Tuple(a',c') | Tuple(a',c'),Pass a -> Tuple(a * a', a * c')
        // | Fail c, Tuple(a',c') | Tuple(a',c'),Fail c -> Tuple(c * a', c * c')
        // | Tuple(a,c),Tuple(b,d) -> Tuple(a * b,c * d)
    mult x y   
// let inline count x y = 
//     let addCounts r1 r2 =
//           let toCount result =  
//                 match result with | Pass _ -> Tuple (1,0) | Fail _ ->  Tuple(0,1) | Tuple _ as x -> x | _ -> failwith "Cannot count these" 
//           add r1 (toCount r2)  
//     addCounts x y          
let inline map f x = 
    let rec map f x = 
        match x with 
            | Pass a -> Pass (f a) 
            | Fail a -> Fail (f a) 
            // | List a -> List.map (map f) a |> List  
            // | Tuple(a,b) -> Tuple(f a,f b)
    map f x
    
let rec bind f x = 
    match x with 
    | Pass x -> f x  
    | Fail x -> Fail x
    // | List xs -> List.map (bind f) xs |> List
    // | Tuple (x,_) -> f x

let either a b =  
    match a,b with 
    | Fail a, Fail _    -> Fail a
    | ((Pass _  as a), _) 
    | (_, (Pass _ as a))      -> a
    
let all a b =  
    match a,b with 
    | ((Pass _ as a), (Pass _ )) -> a
    | ((Fail _ as a), _) 
    | (_, (Fail _ as a))      -> a

let rec (|IsPass|_|) x = 
    match x with 
    | Fail _ -> None
    | Pass y -> Some y
let rec (|IsFail|_|) x = 
    match x with 
    | Fail y -> Some y 
    | _ -> None

let (|CheckValue|) = function
    | Pass p -> p
    | Fail f -> f
let rec printCheckF = 
        function
        | Pass x   -> sprintf "Pass %.2f" x 
        | Fail x  -> sprintf "Fail %.2f" x
        // | List xs -> List.map printCheckF xs |> String.concat ";" |> sprintf "[%s]"
        // | Tuple(x,y) -> sprintf "Passes: %.2f, Fails: %.2f" x y
let rec printCheckD = 
        function
        | Pass x   -> sprintf "Pass %d" x 
        | Fail x  -> sprintf "Fail %d" x
        // | List xs -> List.map printCheckD xs |> String.concat ";" |> sprintf "[%s]"
        // | Tuple(x,y) -> sprintf "Passes: %d,Fails: %d" x y
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
