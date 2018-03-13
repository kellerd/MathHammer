module GameActions.GameActionsList.Types
open GameActions.Primitives.Types
type Display = 
    | Icon of string 
    | Special of string 
    | Text of string option
type Row = 
    | ReadOnly of string * Display * Operation
    | ReadWrite of string * Display * Operation

type Model = 
    { Dragging : string option
      Functions : Row list 
      Editing : bool }

type Msg = 
    // | ModelMsg of MathHammer.Models.Types.Msg * string
    | AddRow 
    | ChangeNewRowName of string
    | ChangeIcon of string
    | SaveOp of string
    | EditRow of string
    | Dragging of string
    | Dragged of string * Operation