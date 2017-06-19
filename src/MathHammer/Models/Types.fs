module MathHammer.Models.Types


let rnd = System.Random()


type SequenceItem<'a> = 
    | Absolute of 'a

type Die =
    | D3
    | D6

type GamePrimitive = 
    | Dice of Die * int
    | Value of int
    | DPlus of int 
    | NoValue 
type Action = 
    | Characteristic of GamePrimitive
    | Ability of GamePrimitive
let isCharacteristic = function Characteristic x -> true | Ability x -> false


type Model = {posX:float; posY:float; name:string; attributes:list<string*Action>; }


type Msg = 
    | ChangePosition of float * float
    | Select
