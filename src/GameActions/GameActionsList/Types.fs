module GameActions.GameActionsList.Types
open GameActions.Primitives.Types
type Display = 
    | Icon of string 
    | Special of string 
    | Text of string option
type Row = 
    | ReadOnly of string * Display * Operation
    | ReadWrite of string * Display * Operation

type Model = Row list * bool

type Msg = 
    // | ModelMsg of MathHammer.Models.Types.Msg * string
    | AddRow 
    | ChangeNewRowName of string
    | ChangeIcon of string
    | SaveOp of string
    | EditRow of string