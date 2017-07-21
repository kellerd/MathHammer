module MathHammer.Models.Types
open GameActions.Primitives.Types
open Distribution 
open Result
type Environment = Map<(Scope*string),Operation>
type Order = int
type Model = { PosX:float
               PosY:float 
               Name:string
               Scale:string
               Size:int<mm>
               Attributes:Map<string,Order*Operation>
               Environment:Environment }


type Msg = 
    | ChangePosition of float * float * string
    | Rebind of Scope * Environment
    | Select
    | Let of Scope * string * Operation
