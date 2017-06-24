module GameActions.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish.React
open Types

let root model dispatch =
    GameActions.GameActionsList.View.root model.Actions dispatch 