module MathHammer.UnitList.Types
open GameActions.Primitives.Types
type Area  = {Top:int<mm>;Left:int<mm>;Width:int<mm>;Height:int<mm>}
type Location = {Label:string; Fill:string; Dimensions: Area}
type Model = {
    Location: Location
    Models:Map<string,MathHammer.Models.Types.Model>
    DeploymentFill:string
    ElementFill:string
    ElementStroke:string
    Deployment : Area
}

type Msg = 
    | ModelMsg of MathHammer.Models.Types.Msg * string
    | Distribute 
