module MathHammer.Models.Types

open GameActions.Primitives.Types

type Environment = Map<string, Operation>

type Order = int

type Model =
    { PosX : float
      PosY : float
      Name : string
      Size : int<mm>
      Attributes : Map<string, Order * Operation>
      Rules : Operation
      NormalizedRules : Operation option
      ProbabilityRules : Operation option }

type Msg =
    | ChangePosition of float * float
    | MakeChoice of string * Set<string>
    | Rebind of Environment
    | Select
