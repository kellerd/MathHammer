module MathHammer.Types
open Result
open GameActions.Primitives.Types
open MathHammer.Models.Types
open Distribution
type Model = {
    Attacker : MathHammer.UnitList.Types.Model
    Defender : MathHammer.UnitList.Types.Model
    Board : int<ft> * int<ft>
    SelectedAttacker : Option<string>
    SelectedDefender : Option<string>
    Environment : Environment
    GlobalOperations : Map<string,Order*Operation>
}

type Msg = 
    | UnitListMsg of MathHammer.UnitList.Types.Msg * string option
    | Swap
    | RebindEnvironment 
    | BindDefender
    | BindAttacker

