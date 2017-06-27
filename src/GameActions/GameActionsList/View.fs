module GameActions.GameActionsList.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types


let mkRows dispatch row  = 
    match row with 
    | ReadOnly (name, gameAction) -> 
        tr [] [
            td [] []
            td [] [str name]
            td [] [GameActions.Primitives.View.root gameAction dispatch]
            td [] [GameActions.Primitives.View.alternateRoot gameAction dispatch]
        ]
    | New(text,op) -> 
        tr [] [
            td [] (GameActions.Primitives.View.root op dispatch :: 
                   [ a [ ClassName "button fa fa-floppy-o"
                         OnClick (fun _ -> SaveOp(text) |> dispatch)  ] [str "Save"] ])
            td [] [
                input [ ClassName "input"
                        Type "text"
                        Placeholder "Type your name"
                        DefaultValue !^text
                        AutoFocus true
                        OnChange (fun ev -> !!ev.target?value |> ChangeNewRowName |> dispatch ) ] ]
            td [] [GameActions.Primitives.View.root op dispatch]
            td [] [GameActions.Primitives.View.alternateRoot op dispatch]
        ]

let root ((model,addButton):Model) dispatch =
    tr [] [
        th [] [
            (if addButton then a [ ClassName "button fa fa-plus-circle"; OnClick (fun _ -> AddRow |> dispatch) ] [] 
            else str "")
        ]        
        th [] [ str "Action Name" ]
        th [] [ str "Equation / Steps"]
        th [] [ str "Editor"]
    ] ::  (model |> List.map (mkRows dispatch)) 
    |> table [ClassName "table"]


      
