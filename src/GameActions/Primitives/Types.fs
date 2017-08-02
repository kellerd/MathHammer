module GameActions.Primitives.Types
open Result    

type Die =
    | D3
    | D6
    | Reroll of (int list) * Die
type GamePrimitive =
    | Int of int
    | Str of string
    | Result of Result<GamePrimitive>
    | NoValue 
    | DPlus of Die * int 
    | Dice of Die
    | Dist of Distribution.Distribution<GamePrimitive>
    | ManyOp of ManyOp
and Operation = 
    | Call of Call
    | Value of GamePrimitive
    | Var of string
    | App of f:Operation * value:Operation
    | Lam of param:string * body:Operation
    | Let of string * value:Operation * body:Operation
and Call = 
    | Product
    | Total
    | Count
    | Unfold 
and ManyOp =
    | OpList of Operation list
    
let rec (|IsDistribution|_|) = function
    | Value(Dist(d)) | Let(_,IsDistribution(d),_) -> Some d
    | _ -> None

let (|IntResult|_|) = function 
    | Int(i) 
    | Result(Pass(Int(i))) -> Pass i |> Some 
    | Result(Fail(Int(i))) -> Fail i |> Some
    | Str(_) -> None
    | Result(_) -> None
    | NoValue -> Fail 0 |> Some
    | DPlus _ -> None
    | Dice(_) -> None
    | Dist(_) -> None
    | ManyOp(_) -> None 

type GamePrimitive with 
    static member Zero = NoValue
    static member (+) (x,y) = 
        match (x,y) with 
        | NoValue,z | z,NoValue -> z
        | Int(a),Int(b) -> Int(a+b)
        | Str(a),Str(b) -> Str(a+b)
        | Dist d, Dist d2 -> Distribution.combine [d;d2] |> Dist
        | Result (r1),Result(r2) -> Result.add r1 r2 |> Result
        | x,y -> failwith <| sprintf "Cannot add these two primitives %A, %A" x y
    static member (*) (x,y) = 
        let rec cartSeq (nss:seq<#seq<'a>>) = 
              let f0 (n:'a) (nss:seq<#seq<'a>>) = 
                match Seq.isEmpty nss with
                | true -> Seq.singleton(Seq.singleton n)
                | _ -> Seq.map (fun (nl:#seq<'a>)->seq{yield n; yield! nl}) nss
              match Seq.isEmpty nss with
              | true -> Seq.empty
              | _ -> Seq.collect (fun n->f0 n (cartSeq (Seq.skip 1 nss))) (Seq.head nss)
        match (x,y) with 
        | NoValue,z | z,NoValue -> NoValue
        | Int(a),Int(b) -> Int(a*b)
        | Str(a),Str(b) -> 
            cartSeq [a.ToCharArray();b.ToCharArray()] 
            |> Seq.map(string)
            |> String.concat "\n"
            |> Str
        | Result r1, Result r2 -> Result.mult r1 r2 |> Result
        | Dist d, Dist d2 -> Distribution.cartesian d d2 |> Distribution.map (fun (d1,d2) -> ManyOp(OpList[Value(d1);Value(d2)])) |> Dist
        | x,y -> failwith <| sprintf "Cannot multiply these two primitives %A, %A" x y
        
        
type NormalizedOperation = Normal | Next of Operation    

type [<Measure>] ft 
and [<Measure>] inch
and [<Measure>] mm

type ft with    
    static member ToInch(a:int<ft>) : int<inch> = a * 12<inch/ft>
    static member ToMM(a:int<ft>) : int<mm> = a * 305<mm/ft>
    static member FromInch(a:int<inch>) : int<ft> = a / 12<inch/ft>
    static member FromMM(a:int<mm>) : int<ft> = a / 305<mm/ft>
type inch with     
    static member ToFt : int<inch>->int<ft> = ft.FromInch
    static member ToMM(a:int<inch>) : int<mm> = a * 25<mm/inch>
    static member FromMM(a:int<mm>) : int<inch> = a / 25<mm/inch>
    static member FromFt : int<ft>->int<inch> = ft.ToInch
type mm with 
    static member ToInch : int<mm>->int<inch> = inch.FromMM
    static member ToFt : int<mm>->int<ft>= ft.FromMM
    static member FromInch : int<inch>->int<mm>= inch.ToMM
    static member FromFt : int<ft>->int<mm> = ft.ToMM

                          
type Msg =  Unit  


