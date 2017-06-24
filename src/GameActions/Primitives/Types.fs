module GameActions.Primitives.Types

type Die =
    | D3
    | D6
    | Reroll of (int list) * Die

type GamePrimitive =
    | Int of int
    | Dice of Die

type Operation = 
    | Many of Operation * int
    | Total of (Operation list)
    | Value of GamePrimitive
    | DPlus of Die * int 
    | NoValue 

type Ability = Operation

                          
type Msg =  Unit  

type Result = Pass of float | Fail of float | List of Result list | Tuple of int * int

let rec divide (x,y) = 
        match x with 
        | Pass a -> Pass (a/y) 
        | Fail a -> Fail (a/y) 
        | List a -> List.map(fun r -> divide(r,y)) a |> List  
        | Tuple(a,b) -> Tuple(System.Math.Ceiling(float a * y) |> int,System.Math.Ceiling(float b * y) |> int)
let rec printResult = function
    | Pass x   -> sprintf "%.2f" x 
    | Fail x  -> sprintf "%.2f" x
    | List xs -> List.map printResult xs |> String.concat ";" |> sprintf "[%s]"
    | Tuple(x,y) -> sprintf "%d,%d" x y

                    
type Result with    
    static member (+) (x:Result,y:float) = 
        let rec add x y = 
            match x with 
            | Pass a -> Pass (a+y) 
            | Fail a -> Fail (a+y) 
            | List a -> List.map(fun r -> add r y) a |> List  
            | Tuple(a,b) -> Tuple(System.Math.Ceiling(float a + y) |> int,System.Math.Ceiling(float b + y) |> int)
        add x y             
    static member (+) (x:Result,y:Result) = 
        let rec add = function 
                      | Pass a, Pass b -> Pass (a + b)
                      | Pass a, Fail _ | Fail _, Pass a  -> Pass (a)
                      | Fail a, Fail b -> Fail (a + b)
                      | List a, List b -> List.map2 (fun r1 r2 -> add (r1,r2)) a b |> List
                      | a, List b | List b, a -> List.map (fun r1 -> add (r1,a)) b |> List
                      | Pass a, Tuple(a',c) | Tuple(a',c),Pass a -> Tuple(a' + int a,c + int a)
                      | Fail a, Tuple(a',c) | Tuple(a',c),Fail a -> Tuple(a',c)
                      | Tuple(a,c),Tuple(b,d) -> Tuple(a+b,c+d)
        add (x,y)              
    static member (*) (x:Result,y) = 
        let rec mult (x,y) = 
            match x with 
            | Pass a -> Pass (a*y) 
            | Fail a -> Fail (a*y) 
            | List a -> List.map(fun r -> mult(r,y)) a |> List  
            | Tuple(a,b) -> Tuple(System.Math.Ceiling(float a * y) |> int,System.Math.Ceiling(float b * y) |> int)
        mult(x,y)   

    static member (/) (x,y) = divide(x,y)    
    static member DivideByInt (x,y:int) = divide(x,float y)
    static member Zero : Result = Pass 0.



let resultListAverage (list:list<Result>) =
    match list with 
    | [] -> Result.Zero
    | xs ->
        let mutable sum = Result.Zero
        let mutable count = 0
        for x in xs do
            sum <- x + sum
            count <- count + 1
        Result.DivideByInt (sum,count)