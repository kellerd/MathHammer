module GameActions.GameActionsList.Types
open GameActions.Primitives.Types
type Row = 
    | ReadOnly of string * Operation
    | ReadWrite of string * Operation

type Model = Row list * bool

type Msg = 
    // | ModelMsg of MathHammer.Models.Types.Msg * string
    | AddRow 
    | ChangeNewRowName of string
    | SaveOp of string
    | EditRow of string