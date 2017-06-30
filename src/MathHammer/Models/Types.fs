module MathHammer.Models.Types
open GameActions.Primitives.Types
type ModelAttribute = 
    | Ability of Ability
    | Characteristic of Ability
type Model = { PosX:float
               PosY:float 
               Name:string
               Scale:string
               Size:int<mm>
               Attributes:list<string*ModelAttribute>
               ShootingRange:Operation
               MeleeRange:Operation }


type Msg = 
    | ChangePosition of float * float * string
    | Select
