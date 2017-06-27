module GameActions.GameActionsList.Types
open GameActions.Primitives.Types
type Row = 
    | ReadOnly of string * Ability
    | New of string * Ability

type Model = Row list * bool

type Msg = 
    // | ModelMsg of MathHammer.Models.Types.Msg * string
    | AddRow 
    | ChangeNewRowName of string
    | SaveOp of string
