module GameActions.GameActionsList.View
open Fable.Core.JsInterop
open Fable.Helpers.React
open Props
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.View
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
let unparseEquation icons operation : Fable.Import.React.ReactElement list = 
    let rec unparseV = function   
        | Int(i) -> [string i |> str]
        | Float(f) -> [sprintf "%.1f" f |> str]
        | Dist(d) -> [unparseDist unparseV d]
        | NoValue -> [str "--" ] 
        | Str s -> [b  [] [str s]]
        | Tuple(v,v2) -> paren <| unparseV v@(str ",")::unparseV v2
        | Check c -> [unparseCheck unparseV c]
        | ParamArray ([]) ->  []
        | ParamArray ([Value (Str _); Value(NoValue)]) ->  []
        | ParamArray ([Value (Str _); Var _]) ->   []
        | ParamArray(m) -> List.collect unparseEq m
    and unparseEq op : Fable.Import.React.ReactElement list = 
        match op with 
        | Call f -> [unparseCall f]
        | Value(v)-> unparseV v
        | Var (v) -> 
            match icons |> Map.tryFind v with 
            | Some icon -> [b [] [icon]]
            | None -> [str v]
        | Lam(_) -> []
        | Choice(name, _) -> [str <| "Choose a " + name]
        | App(Lam(_,x),_) -> unparseEq x 
        | App(f,(Var(v))) -> unparseEq f @ [ofList [sprintf "%s" v |> str]]
        | App(f,a) -> unparseEq f @ [ofList (unparseEq a)]
        | Let(_, _, inner) ->  unparseEq inner
        | PropertyGet(s,op) -> unparseEq op @ [str <| sprintf ".%s" s]
        | IfThenElse(ifExpr, thenExpr, elseExpr) -> 
            let ifPart = str "if " :: unparseEq ifExpr
            let thenPart = str " then " :: br [] :: unparseEq thenExpr
            let elsePart = Option.map(fun elseExpr -> br [] :: unparseEq elseExpr) elseExpr |> Option.toList |> List.collect id
            ifPart @ thenPart @ elsePart   
    unparseEq operation 
let mkRowDrag dispatch  = function
    | ReadOnly (name, icon, _) | ReadWrite(name, icon, _) -> 
        iconToDisplay icon
        |> Option.map (List.singleton >> div [Draggable true; OnDragStart (fun _ -> Dragging(name) |> dispatch)])
        |> ofOption
    
let mkRows hideAddButton dispatch icons row = 
    let iconOptional name icon = 
        match icon |> iconToDisplay with 
        | Some icon -> Map.add name icon icons, [icon]
        | None -> icons, []
    match row with 
    | ReadOnly (name, icon, gameAction) -> 
        let newIcons,iconDisplay = iconOptional name icon
        tr [] [
            td [] [(if hideAddButton then str "" 
                    else  a [ ClassName "button fa fa-pencil-square-o"; OnClick (fun _ -> EditRow(name) |> dispatch) ] [str "Edit"] )]
            td [] [str name]
            td [] iconDisplay
            td [] (unparseEquation icons gameAction) //dispatch)
        ], newIcons
    | ReadWrite(name,icon,op) -> 
        let newIcons,_ = iconOptional name icon
        tr [] [
            td [] [ a [ ClassName "button fa fa-floppy-o"
                        OnClick (fun _ -> SaveOp(name) |> dispatch)  ] [str "Close"] ]
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
            td [] (unparseEquation icons op) ], newIcons
let root model dispatch =
    let draggables = (p [] [str "Common"; br [] ; str "Functions"] )::(List.map (mkRowDrag dispatch) model.Functions)
    let (tableRows,_) = List.mapFold (mkRows model.Editing dispatch) Map.empty<_,_> model.Functions
    [
        div [ClassName "column is-11"] [
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
                    tbody [] tableRows ] ]
        div [ClassName "column is-1 has-text-centered"] draggables ]
    |> ofList                    

    

      
