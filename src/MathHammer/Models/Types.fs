module MathHammer.Models.Types


let rnd = System.Random()


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
    | Sum of (GamePrimitive * GamePrimitive)
    | Value of GamePrimitive
    | DPlus of Die * int 
    | NoValue 
type Action = 
    | Characteristic of Operation
    | Ability of Operation
let isCharacteristic = function Characteristic x -> true | Ability x -> false


type Model = {posX:float; posY:float; name:string; attributes:list<string*Action>; }


type Msg = 
    | ChangePosition of float * float
    | Select
