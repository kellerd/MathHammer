module MathHammer.UnitList.Types
open GameActions.Primitives.Types
type Area  = {Top:int<ft>;Left:int<ft>;Width:int<ft>;Height:int<ft>}

type Model = {
    OffsetY:int<mm>
    Models:Map<string,MathHammer.Models.Types.Model>
    BoxFill:string
    DeploymentFill:string
    ElementFill:string
    ElementStroke:string
    Scale : string
    Name : string
    Width : int<mm>
    Height : int<mm>
    Deployment : int<mm>
}

type Msg = 
    | ModelMsg of MathHammer.Models.Types.Msg * string
    | Distribute 
