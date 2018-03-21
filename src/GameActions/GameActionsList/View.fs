module GameActions.GameActionsList.View
open Fable.Core.JsInterop
open Fable.Helpers.React
open Props
open GameActions.Primitives.Types
open GameActions.Primitives.View
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
        |> Option.map (List.singleton >> 
                       div [ Style [Cursor "move"]
                             Draggable true 
                             OnDragEnd (fun _ -> dispatch DragLeft)
                             OnDragStart (fun _ -> Dragging(name) |> dispatch)])
        |> ofOption
    
let mkRows dragging hideAddButton (dispatch:Msg->unit) icons row = 
    let unparseEquation dragging operation dispatch = 
        let rec unparseV (dispatch:GamePrimitive->unit) v  = 
            match v with    
            | Int(i) -> [string i |> str]
            | Float(f) -> [sprintf "%.1f" f |> str]
            | Dist(d) -> [unparseDist (unparseV dispatch) d]
            | NoValue -> [str "--" ] 
            | Str s -> [b  [] [str s]]
            | Tuple(v,v2) -> paren (unparseV (fun gp -> Tuple(gp,v2) |> dispatch) v ) @(str ",")::unparseV (fun gp -> Tuple(v,gp) |> dispatch) v2 
            | Check c -> [unparseCheck (unparseV dispatch) c]
            | ParamArray ([]) ->  []
            | ParamArray ([Value (Str _); Value(NoValue)]) ->  []
            | ParamArray ([Value (Str _); Var _]) ->   []
            | ParamArray(ops) -> 
                ops 
                |> Zipper.permute 
                |> List.collect(function 
                                | Empty -> [] 
                                | Zipper(l,a,r) -> unparseEq a (fun op' -> l @ op'::r |> ParamArray |> dispatch)) 
        and unparseApp f a dispatch = 
            let (joinStr) = 
                match f  with 
                | Call Product      -> str " * "
                | Call Division     -> str " / "
                | Call Count        -> str ", "
                | Call Repeat       -> str " × "
                | Call (Dice _)     -> ofOption None
                | Call GreaterThan  -> str ">"
                | Call Equals       -> str "=="
                | Call NotEquals    -> str "!="
                | Call LessThan     -> str "<"
                | Call And          -> str " && "
                | Call Or           -> str " || "
                | Call Max          -> str ", "
                | Call Min          -> str ", "
                | Call Sub          -> str " - "
                | Call Median       -> str ", "
                | Call Mean         -> str ", "
                | Call Mode         -> str ", "
                | Call (Least   _)  -> str ", "
                | Call (Largest _)  -> str ", "            
                | Call Total        -> str " + " 
                | _                 -> ofOption None
            match a with 
            | Value(ParamArray ops) -> 
                let call = unparseEq f (fun op -> (op,a) |> dispatch)
                let ops' = 
                    ops 
                    |> Zipper.permute 
                let param = 
                    List.foldBack (function 
                                    | Empty -> id
                                    | Zipper(l,a,r) -> fun acc -> 
                                        let reset ( e : Fable.Import.React.DragEvent) = 
                                            e.target?className <- "has-text-centered"
                                            e.target?style?border <- "3px dotted black"
                                            e.target?style?``background-color`` <- ""
                                            e.target?style?width <- "18px"
                                            e.target?style?height <- "18px"
                                        let tail = 
                                            let newValuePlaceholder = 
                                                Option.map (fun dragging -> 
                                                    [ span [ ] [ joinStr ]
                                                      span [ OnDragOver (fun e -> 
                                                                            e.preventDefault()
                                                                            e.dataTransfer.dropEffect <- "move"
                                                                            e.target?className <- "has-text-centered"
                                                                            e.target?style?border <- "3px dotted green"
                                                                            e.target?style?``background-color`` <- "#0088CC"
                                                                            e.target?style?width <- "24px"
                                                                            e.target?style?height <- "24px")
                                                             OnDragLeave reset
                                                             OnDrop (fun e -> 
                                                                reset e 
                                                                (f, List.append ops [Var dragging] |> ParamArray |> Value) |> dispatch )
                                                             Class ("has-text-centered")
                                                             Style [ StrokeWidth 1.
                                                                     Border "dotted"
                                                                     Margin "3px"
                                                                     Width  "18px"
                                                                     Height "18px"
                                                                     Display "inline-block" ] ] [] ]
                                                    |> ofList ) dragging                                                          
                                            match acc with 
                                            | [] -> Option.toList newValuePlaceholder
                                            | _ -> joinStr :: acc
                                        unparseEq a (fun op' -> (f,l @ op'::r |> ParamArray |> Value) |> dispatch) @ tail ) ops' [] 
                call  @ param
            | Value(NoValue) -> unparseEq f (fun op -> (op,a) |> dispatch)
            | _ -> unparseEq f (fun op -> (op,a) |> dispatch) @ unparseEq a (fun op -> (f,op) |> dispatch)  
        and unparseChoice dispatch (choices:(string*Operation) list) =
            choices 
            |> Zipper.permute  
            |> List.collect(function 
                            | Empty -> [] 
                            | Zipper(l,a,r) -> 
                                     str (fst a + ":= ") :: 
                                     br [] ::
                                     unparseEq (snd a) (fun op' -> l @ (fst a,op')::r |> dispatch))  
        and unparseC dispatch func = 
            match func with 
            | Dice(i) -> string i  |> str
            | Count -> sprintf "(Passes,Fails) in " |> str
            | _  -> sprintf "%A" func |> str  
        and unparseEq op (dispatch:Operation->unit) : Fable.Import.React.ReactElement list = 
            match op with 
            | Call f -> [unparseC (Call >> dispatch) f]
            | Value(v)-> unparseV (Value >> dispatch) v
            | Var (v) -> 
                match icons |> Map.tryFind v with 
                | Some icon -> [b [] [icon]]
                | None -> [str v]
            | Lam(x,body) -> str (x + " => ") :: (unparseEq body (fun op -> Lam(x, op) |> dispatch))
            | Choice(name, choices) ->  
                [ str <| name + "one of: "
                  br []
                  ul [] (unparseChoice (fun ch -> Choice(name, ch) |> dispatch) choices ) ]
            | App(f,a) -> unparseApp f a (App >> dispatch)
            | Let(x, v, inner) ->  
                [ str ("let " + x + " = ") :: unparseEq v (fun op -> Let(x, op, inner) |> dispatch)
                  [br []]
                  unparseEq inner (fun op -> Let(x, v, op) |> dispatch) ]
                |> List.collect id
            | PropertyGet(s,op) -> unparseEq op (fun op -> PropertyGet(s,op) |> dispatch ) @ [str <| sprintf ".%s" s]
            | IfThenElse(ifExpr, thenExpr, elseExpr) -> 
                let ifPart = str "if " :: unparseEq ifExpr (fun op -> IfThenElse(op, thenExpr, elseExpr) |> dispatch )
                let thenPart = str " then " :: br [] :: unparseEq thenExpr (fun op -> IfThenElse(ifExpr, op, elseExpr) |> dispatch )
                let elsePart = Option.map(fun elseExpr -> br [] :: unparseEq elseExpr (fun op -> IfThenElse(ifExpr, thenExpr, Some op) |> dispatch )) elseExpr |> Option.toList |> List.collect id
                ifPart @ thenPart @ elsePart   
        unparseEq operation dispatch
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
            td [Style [Position "relative"]] [unparseEquation None gameAction ignore |> div [Class "columns" ]] //dispatch)
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
            td [] (unparseEquation dragging op (fun op -> Dragged(name,op) |> dispatch)) ], newIcons
let root model dispatch =
    let draggables = (p [] [str "Common"; br [] ; str "Functions"] )::(List.map (mkRowDrag dispatch) model.Functions)
    let (tableRows,_) = List.mapFold (mkRows model.Dragging model.Editing dispatch) Map.empty<_,_> model.Functions
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

    

      
