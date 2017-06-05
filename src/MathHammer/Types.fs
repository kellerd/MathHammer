module MathHammer.Types

type Model = {
    Attacker : MathHammer.UnitList.Types.Model
    Defender : MathHammer.UnitList.Types.Model
    Selected : Option<MathHammer.Models.Types.Model>
}

type Msg = 
    | UnitListMsg of MathHammer.UnitList.Types.Msg * string option
    | Swap
