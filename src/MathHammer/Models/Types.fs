module MathHammer.Models.Types
open GameActions.Primitives.Types
type Environment = Map<string,Operation>
type Order = int
type Model = { PosX:float
               PosY:float 
               Name:string
               Scale:string
               Size:int<mm>
               Attributes: Operation list
               Rules: Operation
               NormalizedRules:Operation
               ProbabilityRules:Operation }


type Msg = 
    | ChangePosition of float * float * string
    | Rebind of Environment
    | Select
