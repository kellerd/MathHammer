module MathHammer.Types
open Result
open GameActions.Primitives.Types
type Model = {
    Attacker : MathHammer.UnitList.Types.Model
    Defender : MathHammer.UnitList.Types.Model
    Board : int<ft> * int<ft>
    Selected : Option<MathHammer.Models.Types.Model>
    StoredActions : Map<string,Probability.Distribution<Result>>
}

type Msg = 
    | UnitListMsg of MathHammer.UnitList.Types.Msg * string option
    | Swap
