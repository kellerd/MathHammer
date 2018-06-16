module MathHammer.Types

open GameActions.Primitives.Types
open Models.Types
open MathHammer.UnitList.Types

type Mode =
    | Probability
    | Average
    | Sample
    override x.ToString() =
        match x with
        | Probability -> "Probabilities"
        | Average -> "Averages"
        | Sample -> "Sample"

type Matchup =
    { Attacker : string
      Defender : string option }

type Model =
    { Attacker : UnitList.Types.Model
      Defender : UnitList.Types.Model
      Board : Area
      SelectedAttacker : Option<string>
      SelectedDefender : Option<string>
      Environment : Environment
      GlobalOperations : (string * (Order * Operation)) list
      Choices : Map<string, Set<string>>
      SelectedChoices : Map<string, string>
      Matchups : Map<Matchup, Operation>
      Dragging : Option<float * float> * Option<string * string>
      Mode : Mode }

type Msg =
    | UnitListMsg of UnitList.Types.Msg * string option
    | StartDrag of float * float
    | EndDrag
    | Drag
    | Choose of string * string
    | Swap
    | ChangeMode of Mode
    | RebindEnvironment
    | BindDefender
    | BindAttacker
