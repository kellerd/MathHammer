module MathHammer.Types
open Result
open GameActions.Primitives.Types
type Model = {
    Attacker : MathHammer.UnitList.Types.Model
    Defender : MathHammer.UnitList.Types.Model
    Board : int<ft> * int<ft>
    SelectedAttacker : Option<MathHammer.Models.Types.Model>
    SelectedDefender : Option<MathHammer.Models.Types.Model>
    Environment : Map<(Environment*string),Probability.Distribution<Result.Result>>
}

type Msg = 
    | UnitListMsg of MathHammer.UnitList.Types.Msg * string option
    | Swap
    | RebindEnvironment
    | BindDefender
    | BindAttacker

