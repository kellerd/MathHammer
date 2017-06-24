module GameActions.GameActionsList.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types




let root (model:Model) dispatch =
    model 
    |> List.map (fun (name,gameAction) -> 
        tr [] [
            td [] [str name]
            td [] [GameActions.Primitives.View.root gameAction dispatch]
        ]

    ) |> table []


      
