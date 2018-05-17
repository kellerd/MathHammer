module MathHammer.UnitList.Types

open GameActions.Primitives.Types

type Area =
    { Top : int<mm>
      Left : int<mm>
      Width : int<mm>
      Height : int<mm> }

type Location =
    { Label : string option
      Fill : string
      Dimensions : Area }

type DistributeType =
    | Area of Area
    | Enemy of Map<string, MathHammer.Models.Types.Model>

type Model =
    { Location : Location
      Models : Map<string, MathHammer.Models.Types.Model>
      ElementFill : string
      ElementStroke : string
      Deployment : Location }

type Msg =
    | ModelMsg of MathHammer.Models.Types.Msg * string
    | Distribute of DistributeType
