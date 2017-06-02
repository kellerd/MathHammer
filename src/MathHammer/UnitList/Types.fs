module MathHammer.UnitList.Types

type Model = {
    OffsetY:float
    Models:MathHammer.Models.Types.Model list
    BoxFill:string
    ElementFill:string
    ElementStroke:string
}

type Msg = | Distribute 
