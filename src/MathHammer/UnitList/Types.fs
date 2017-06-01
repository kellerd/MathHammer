module MathHammer.UnitList.Types

type Model = {
    OffsetY:float
    Models:MathHammer.Model.Types.Model list
    BoxFill:string
    ElementFill:string
    ElementStroke:string
}

type Msg = Unit
