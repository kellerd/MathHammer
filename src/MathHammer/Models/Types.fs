module MathHammer.Models.Types
open GameActions.Primitives.Types
open Distribution 
open Result
type Environment = Map<(Scope*string),Distribution<Result<int>>>
type Model = { PosX:float
               PosY:float 
               Name:string
               Scale:string
               Size:int<mm>
               Attributes:Map<string,Operation>
               Environment:Environment }


type Msg = 
    | ChangePosition of float * float * string
    | Rebind of Scope
    | Select
    | Let of Scope * string * Distribution<Result<int>>
