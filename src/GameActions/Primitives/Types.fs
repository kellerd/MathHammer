module GameActions.Primitives.Types

type Die =
    | D3
    | D6
    | Reroll of (int list) * Die
    
type GamePrimitive =
    | Int of int
    | Str of string
    | Pair of GamePrimitive * GamePrimitive
    | NoValue 
    | DPlus of Die * int 
    | Dice of Die
    | Dist of Distribution.Distribution<Result.Result<GamePrimitive>>
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
and ManyOp =
    | OpList of Operation list
    | Unfold of Operation * Operation
let rec (|IsDistribution|_|) = function
    | Value(Dist(d)) | Let(_,IsDistribution(d),_) -> Some d
    | _ -> None

let add x y = 
    match (x,y) with 
    | NoValue,z | z,NoValue -> z
    | Int(a),Int(b) -> Int(a+b)
    | Str(a),Str(b) -> Str(a+b)
    | Dist d, Dist d2 -> Distribution.combine [d;d2] |> Dist
    | _ -> failwith "Cannot add these two primitives"
let mult x y = 
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
        //|> String.concat "" |> String.concat "\n" |> Str
    // | Dist d, Dist d2 -> Distribution.cartesian d d2 |> Distribution.map (Result.Tuple) |> Dist
    | _ -> failwith "Cannot multiply these two primitives"

type GamePrimitive with 
    static member inline(+) (x,y) = add x y
    static member inline(*) (x,y) = mult x y
        
        
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


