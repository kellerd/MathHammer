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
      ProbabilityRules : Operation option
      Choices : Map<string, Set<string>> }

type Msg =
    | ChangePosition of float * float
    | Rebind of Environment
    | Select
