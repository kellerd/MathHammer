module MathHammer.Types
open GameActions.Primitives.Types
open Models.Types
type Model = {
    Attacker : UnitList.Types.Model
    Defender : UnitList.Types.Model
    Board : int<ft> * int<ft>
    SelectedAttacker : Option<string>
    SelectedDefender : Option<string>
    Environment : Environment
    GlobalOperations : Map<string,Order*Operation>
}

type Msg = 
    | UnitListMsg of UnitList.Types.Msg * string option
    | Swap
    | RebindEnvironment 
    | BindDefender
    | BindAttacker

