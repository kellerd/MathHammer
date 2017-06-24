module App.Types

open Global

type Msg =
  | CounterMsg of Counter.Types.Msg
  | HomeMsg of Home.Types.Msg
  | MathHammerMsg of MathHammer.Types.Msg
  | GameActionsMsg of GameActions.Types.Msg

type Model = {
    currentPage: Page
    counter: Counter.Types.Model
    home: Home.Types.Model
    mathHammer:MathHammer.Types.Model
    gameActions:GameActions.Types.Model
  }
