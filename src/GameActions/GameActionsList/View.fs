module GameActions.GameActionsList.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types


let mkRows hideAddButton dispatch row  = 
    match row with 
    | ReadOnly (name, gameAction) -> 
        [tr [] [
            td [] [(if hideAddButton then str "" 
                    else  a [ ClassName "button fa fa-pencil-square-o"; OnClick (fun _ -> EditRow(name) |> dispatch) ] [str "Edit"] )]
            
            td [] [str name] 
            td [] [GameActions.Primitives.View.root gameAction dispatch]
        ]]
    | ReadWrite(text,op) -> 
        [ tr [] [
            td [] [ a [ ClassName "button fa fa-floppy-o"
                        OnClick (fun _ -> SaveOp(text) |> dispatch)  ] [str "Save"] ]
            td [] [ 
                input [ ClassName "input"
                        Type "text"
                        Placeholder "Type the action name"
                        DefaultValue !^text
                        AutoFocus true 
                        OnChange (fun ev -> !!ev.target?value |> ChangeNewRowName |> dispatch ) ] ]
            td [] [GameActions.Primitives.View.root op dispatch] ]
          tr [] [td [ColSpan 3.; ClassName "has-text-centered"] [GameActions.Primitives.View.alternateRoot op dispatch]]]

let root ((model,hideAddButton):Model) dispatch =
    [   thead [] [
            tr [] [
                th [] [
                    (if hideAddButton then str "" 
                    else a [ ClassName "button fa fa-plus-circle"; OnClick (fun _ -> AddRow |> dispatch) ] [str "Add"] )
                ]        
                th [] [ str "Action Name" ]
                th [] [ str "Equation / Steps"] ] ] 
        tbody [] (List.collect (mkRows hideAddButton dispatch) model) 
    ] 
    |> table [ClassName "table"]


      
