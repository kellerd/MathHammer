module MathHammer.Models.Types
open MathHammer.GameActions.Types
type ModelAttribute = 
    | Ability of Ability
    | Characteristic of Ability
type Model = { posX:float
               posY:float 
               name:string 
               attributes:list<string*ModelAttribute> }


type Msg = 
    | ChangePosition of float * float
    | Select
