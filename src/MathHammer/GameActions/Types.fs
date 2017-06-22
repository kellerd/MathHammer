module MathHammer.GameActions.Types

type SequenceItem<'a> = 
    | Absolute of 'a

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


type Result = Pass of float | Fail of float | List of Result list | Tuple of int * int


                    
type Result with
    static member (+) (x,y) = 
        let rec add = function 
                      | Pass a', Pass b' -> Pass (a' + b')
                      | Pass a', Fail b' -> Pass (a')
                      | Fail a', Pass b' -> Pass (b')
                      | Fail a', Fail b' -> Fail (a' + b')
                      | List a', List b' -> (List.map2 (fun r1 r2 -> add (r1,r2)) a' b') |> List
                      | Tuple(a,c),Tuple(b,d) -> Tuple(a+b,c+d)
                      | _ -> failwith "Cannot add this type"
        add (x,y)              
    static member (*) (x,y) = 
        let rec mult (x,y) = 
            match x with 
            | Pass a -> Pass (a*y) 
            | Fail a -> Fail (a*y) 
            | List a -> List.map(fun r -> mult(r,y)) a |> List  
            | Tuple(a,b) -> Tuple(System.Math.Ceiling(float a * y) |> int,System.Math.Ceiling(float b * y) |> int)
        mult(x,y)    
    static member (/) (x,y) = 
        let rec mult (x,y) = 
            match x with 
            | Pass a -> Pass (a/y) 
            | Fail a -> Fail (a/y) 
            | List a -> List.map(fun r -> mult(r,y)) a |> List  
            | Tuple(a,b) -> Tuple(System.Math.Ceiling(float a * y) |> int,System.Math.Ceiling(float b * y) |> int)
        mult(x,y)
                          
type Msg =  Unit  