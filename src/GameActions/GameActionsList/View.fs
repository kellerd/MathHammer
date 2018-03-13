module GameActions.GameActionsList.View
open Fable.Core.JsInterop
open Fable.Helpers.React
open Props
open Types
let d6icon = 
    svg [ SVGAttr.Width 24
          SVGAttr.Height 24
          ViewBox "0 0 24 24"]
        [
              path [SVGAttr.Fill "#000000" 
                    D "M5,3H19A2,2 0 0,1 21,5V19A2,2 0 0,1 19,21H5A2,2 0 0,1 3,19V5A2,2 0 0,1 5,3M7,5A2,2 0 0,0 5,7A2,2 0 0,0 7,9A2,2 0 0,0 9,7A2,2 0 0,0 7,5M17,15A2,2 0 0,0 15,17A2,2 0 0,0 17,19A2,2 0 0,0 19,17A2,2 0 0,0 17,15M17,10A2,2 0 0,0 15,12A2,2 0 0,0 17,14A2,2 0 0,0 19,12A2,2 0 0,0 17,10M17,5A2,2 0 0,0 15,7A2,2 0 0,0 17,9A2,2 0 0,0 19,7A2,2 0 0,0 17,5M7,10A2,2 0 0,0 5,12A2,2 0 0,0 7,14A2,2 0 0,0 9,12A2,2 0 0,0 7,10M7,15A2,2 0 0,0 5,17A2,2 0 0,0 7,19A2,2 0 0,0 9,17A2,2 0 0,0 7,15Z"] []
        ]
let d3icon = 
    svg [ SVGAttr.Width 24
          SVGAttr.Height 24
          ViewBox "0 0 24 24"]
        [
              path [SVGAttr.Fill "#000000" 
                    D "M5,3H19A2,2 0 0,1 21,5V19A2,2 0 0,1 19,21H5A2,2 0 0,1 3,19V5A2,2 0 0,1 5,3M12,10A2,2 0 0,0 10,12A2,2 0 0,0 12,14A2,2 0 0,0 14,12A2,2 0 0,0 12,10M7,5A2,2 0 0,0 5,7A2,2 0 0,0 7,9A2,2 0 0,0 9,7A2,2 0 0,0 7,5M17,15A2,2 0 0,0 15,17A2,2 0 0,0 17,19A2,2 0 0,0 19,17A2,2 0 0,0 17,15Z"] []
        ]
let iconToDisplay  = function 
   | Special "D6" -> d6icon |> Some
   | Special "D3" -> d3icon |> Some
   | Icon s       -> div [ClassName ("fa " + s)] [] |> Some
   | Text(Some s) -> strong [] [str s] |> Some
   | _            -> None      
let mkRowDrag dispatch  = function
    | ReadOnly (name, icon, _) | ReadWrite(name, icon, _) -> 
        iconToDisplay icon
        |> Option.map (List.singleton >> div [OnDragStart (fun _ -> Dragging(name) |> dispatch)])
        |> ofOption
    
let mkRows hideAddButton dispatch row  = 
    match row with 
    | ReadOnly (name, icon, gameAction) -> 
        [tr [] [
            td [] [(if hideAddButton then str "" 
                    else  a [ ClassName "button fa fa-pencil-square-o"; OnClick (fun _ -> EditRow(name) |> dispatch) ] [str "Edit"] )]
            td [] [str name]
            td [] (icon |> iconToDisplay |> Option.toList )
            td [] (GameActions.Primitives.View.probabilities gameAction ignore) //dispatch)
        ]]
    | ReadWrite(name,icon,op) -> 
        [ tr [] [
            td [] [ a [ ClassName "button fa fa-floppy-o"
                        OnClick (fun _ -> SaveOp(name) |> dispatch)  ] [str "Save"] ]
            td [] [ 
                input [ ClassName "input"
                        Type "text"
                        Placeholder "Type the action name"
                        DefaultValue name
                        AutoFocus true 
                        OnChange (fun ev -> !!ev.target?value |> ChangeNewRowName |> dispatch ) ] ]
            td [] [ 
                input [ ClassName "input"
                        Type "text"
                        Placeholder "FA Icon/Special/Text"
                        DefaultValue (icon |> function Special s  | Text(Some s) -> s | Icon s -> s.Remove(0,3) | Text None -> "")
                        OnChange (fun ev -> !!ev.target?value |> ChangeIcon |> dispatch ) ] ]         
            td [] (GameActions.Primitives.View.probabilities op ignore) ] //dispatch) ]
        ]
let root model dispatch =
    [
        div [ClassName "is-11"] [
            table [ClassName "table is-fullwidth"] 
                [   thead [] [
                        tr [] [
                            th [] [
                                (if model.Editing then str "" 
                                else a [ ClassName "button fa fa-plus-circle"; OnClick (fun _ -> AddRow |> dispatch) ] [str "Add"] )
                            ]        
                            th [] [ str "Action Name" ]
                            th [] [ str "Icon / Special Text" ]
                            th [] [ str "Equation / Steps"] ] ] 
                    tbody [] (List.collect (mkRows model.Editing dispatch) model.Functions) ] ]
        div [ClassName "is-1"] (List.map (mkRowDrag dispatch) model.Functions) ]
    |> ofList                    

    

      
