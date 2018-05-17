module App.Types

open Global

type Msg =
    | MathHammerMsg of MathHammer.Types.Msg
    | GameActionsMsg of GameActions.Types.Msg

type Model =
    { currentPage : Page
      mathHammer : MathHammer.Types.Model
      gameActions : GameActions.Types.Model }
