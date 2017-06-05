module MathHammer.Models.Types

type Model = {posX:float;posY:float;name:string;list:string}


type Msg = 
    | ChangePosition of float * float
    | Select
