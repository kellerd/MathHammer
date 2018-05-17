module GameActions.View

open Types
open Fable.Helpers.React
open Fable.Helpers.React.Props

let root model dispatch = div [ ClassName "columns" ] [ GameActions.GameActionsList.View.root model.Actions (GameActionListMsg >> dispatch) ]
