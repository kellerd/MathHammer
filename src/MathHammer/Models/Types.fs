module MathHammer.Models.Types
open GameActions.Primitives.Types
type ModelAttribute = 
    | Ability of Ability
    | Characteristic of Ability
type Model = { posX:float
               posY:float 
               name:string
               size:int<mm>
               attributes:list<string*ModelAttribute>
               shootingRange:Operation
               meleeRange:Operation }


type Msg = 
    | ChangePosition of float * float
    | Select
