module MathHammer.Models.Types
open GameActions.Primitives.Types
open Distribution 
open Result
type Environment = Map<string,Operation>
type Order = int
type Model = { PosX:float
               PosY:float 
               Name:string
               Scale:string
               Size:int<mm>
               Attributes: Operation list
               Rules: Operation
               EvaluatedRules:Operation }


type Msg = 
    | ChangePosition of float * float * string
    | Rebind of Environment
    | Select
    | Let of string * Operation
