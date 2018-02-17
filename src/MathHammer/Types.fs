module MathHammer.Types
open GameActions.Primitives.Types
open Models.Types
type Mode = Probability | Average | Sample with
    override x.ToString() =
        match x with 
        | Probability  -> "Probabilities"
        | Average  -> "Averages"
        | Sample -> "Sample"

type Model = {
    Attacker : UnitList.Types.Model
    Defender : UnitList.Types.Model
    Board : int<ft> * int<ft>
    SelectedAttacker : Option<string>
    SelectedDefender : Option<string>
    Environment : Environment
    GlobalOperations : Map<string,Order*Operation>
    Choices : Map<string, Set<string>>
    SelectedChoices : Map<string,string>
    Mode : Mode
}

type Msg = 
    | UnitListMsg of UnitList.Types.Msg * string option
    | Choose of string * string
    | Swap    
    | ChangeMode of Mode
    | RebindEnvironment 
    | BindDefender
    | BindAttacker

