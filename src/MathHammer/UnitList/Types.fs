module MathHammer.UnitList.Types

type Model = {
    OffsetY:float
    Models:Map<string,MathHammer.Models.Types.Model>
    BoxFill:string
    ElementFill:string
    ElementStroke:string
    Name : string
}

type Msg = 
    | ModelMsg of MathHammer.Models.Types.Msg * string
    | Distribute 
