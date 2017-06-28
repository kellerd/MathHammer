module GameActions.Primitives.Types

type Die =
    | D3
    | D6
    | Reroll of (int list) * Die
    
type Environment =
    | Attacker
    | Defender
    | Global

type GamePrimitive =
    | Int of int
    | Dice of Die


type Operation = 
    | Multiply of Operation * Operation
    | Total of Operation list
    | Count of Operation list
    | Value of GamePrimitive
    | Var of Environment * string
    | DPlus of Die * int 
    | NoValue 
    | Let of Environment * string * Operation

type Ability = Operation

                          
type Msg =  Unit  


