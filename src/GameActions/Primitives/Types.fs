module GameActions.Primitives.Types

type Die =
    | D3
    | D6
    | Reroll of (int list) * Die
    
type Scope =
    | Attacker
    | Defender
    | Global

type GamePrimitive =
    | Int of int
    | NoValue 
    | Dice of Die

type Operation = 
    | Call of Call
    | Value of GamePrimitive
    | Var of Scope * string
    | App of f:Operation * value:Operation
    | Lam of Scope * param:string * body:Operation
    | Let of Scope * string * Operation
and Call = 
    | Product of ManyOp
    | Total of ManyOp
    | Count of ManyOp
    | DPlus of Die * int 
    
and ManyOp =
    | OpList of Operation list
    | Unfold of Operation * Operation
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


