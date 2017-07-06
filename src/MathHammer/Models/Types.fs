module MathHammer.Models.Types
open GameActions.Primitives.Types
open Probability
open Result
type Model = { PosX:float
               PosY:float 
               Name:string
               Scale:string
               Size:int<mm>
               Attributes:list<string*Operation>
               ShootingRange:Operation
               MeleeRange:Operation }


type Msg = 
    | ChangePosition of float * float * string
    | Select
    | Let of Environment * string * Probability.Distribution<Result.Result>
