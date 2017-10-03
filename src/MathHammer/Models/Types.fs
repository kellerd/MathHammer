module MathHammer.Models.Types
open GameActions.Primitives.Types
open Distribution 
open Result
type Environment = Map<string,Operation>
type Order = int
type Model = { PosX:float
               PosY:float 
               Name:string
               Scale:string
               Size:int<mm>
               Attributes: Operation list
               Rules: Operation
               SampleRules:Operation
               NormalizedRules:Operation
               AverageRules:Operation
               ProbabilityRules:Operation }
type DisplayType = 
    | DFloat of float
    | DInt of int
    | DStr of string
    | DResult of Result<DisplayType> 
    | DNoValue
    | DDist of Distribution<DisplayType>
    with static member (+) (a,b) =
                        match a,b with
                        | DNoValue,x | x,DNoValue -> x
                        | DInt a, DInt b -> DInt (a + b)
                        | DFloat a, DFloat b -> DFloat (a + b)
                        | DStr a, DStr b -> DStr (a + b)
                        | DDist d, DDist d2 -> Distribution.combine [d;d2] |> DDist
                        | DResult (r1),DResult(r2) -> Result.add r1 r2 |> DResult
                        | x,y -> failwith <| sprintf "Cannot add display types %A + %A" x y

type Msg = 
    | ChangePosition of float * float * string
    | Rebind of Environment
    | Select
    | Let of string * Operation
