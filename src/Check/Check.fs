[<RequireQualifiedAccess>]
module Check
type Check<'a> = Pass of 'a | Fail of 'a 
let inline combineFavourPass f x y = 
    let rec add x y = 
        match x,y with  
        | Pass a, Pass b -> Pass (f a b)
        | Pass a, Fail _ | Fail _, Pass a  -> Pass (a)
        | Fail a, Fail b -> Fail (f a b)
    add x y
let inline combineFavourFail f x y =
    let rec mult x y =
        match x,y with  
        | Pass a, Pass b -> Pass (f a b)
        | Pass a, Fail b | Fail b, Pass a  -> Fail (f a b)
        | Fail a, Fail b -> Fail (f a b)
    mult x y   
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
let rec printCheckD = 
        function
        | Pass x   -> sprintf "Pass %d" x 
        | Fail x  -> sprintf "Fail %d" x