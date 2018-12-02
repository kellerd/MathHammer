module GameActions.GameActionsList.View

open Fable.Core.JsInterop
open Fable.Helpers.React
open Props
open GameActions.Primitives.Types
open GameActions.Primitives.TypeChecker
open GameActions.Primitives.View
open Types

let d6icon =
    svg [ SVGAttr.Width 18
          SVGAttr.Height 18
          ViewBox "0 0 24 24" ] 
        [ path 
              [ SVGAttr.Fill "#000000"
                
                D 
                    "M5,3H19A2,2 0 0,1 21,5V19A2,2 0 0,1 19,21H5A2,2 0 0,1 3,19V5A2,2 0 0,1 5,3M7,5A2,2 0 0,0 5,7A2,2 0 0,0 7,9A2,2 0 0,0 9,7A2,2 0 0,0 7,5M17,15A2,2 0 0,0 15,17A2,2 0 0,0 17,19A2,2 0 0,0 19,17A2,2 0 0,0 17,15M17,10A2,2 0 0,0 15,12A2,2 0 0,0 17,14A2,2 0 0,0 19,12A2,2 0 0,0 17,10M17,5A2,2 0 0,0 15,7A2,2 0 0,0 17,9A2,2 0 0,0 19,7A2,2 0 0,0 17,5M7,10A2,2 0 0,0 5,12A2,2 0 0,0 7,14A2,2 0 0,0 9,12A2,2 0 0,0 7,10M7,15A2,2 0 0,0 5,17A2,2 0 0,0 7,19A2,2 0 0,0 9,17A2,2 0 0,0 7,15Z" ] 
              [] ]

let d3icon =
    svg [ SVGAttr.Width 18
          SVGAttr.Height 18
          ViewBox "0 0 24 24" ] 
        [ path 
              [ SVGAttr.Fill "#000000"
                
                D 
                    "M5,3H19A2,2 0 0,1 21,5V19A2,2 0 0,1 19,21H5A2,2 0 0,1 3,19V5A2,2 0 0,1 5,3M12,10A2,2 0 0,0 10,12A2,2 0 0,0 12,14A2,2 0 0,0 14,12A2,2 0 0,0 12,10M7,5A2,2 0 0,0 5,7A2,2 0 0,0 7,9A2,2 0 0,0 9,7A2,2 0 0,0 7,5M17,15A2,2 0 0,0 15,17A2,2 0 0,0 17,19A2,2 0 0,0 19,17A2,2 0 0,0 17,15Z" ] 
              [] ]
let (|HasIcon|_|) =
    function 
    | Special "D6" -> d6icon |> Some
    | Special "D3" -> d3icon |> Some
    | Icon s -> div [ ClassName("fa " + s) ] [] |> Some
    | Text(Some s) -> strong [] [ str s ] |> Some
    | _ -> None

let tags tags =
    div [ ClassName "tags is-marginless is-medium has-addons " ] tags
let tag tagClass item =
    span [ ClassName("tag is-marginless is-medium " + tagClass) ] [ item ]
let tagGroup tags = div [] [ tags ]

//tags |> List.singleton |> div [Class "field is-grouped is-grouped-multiline"]  
let draggable dispatch name item =
    item
    |> List.singleton
    |> div [ Style [ Cursor "move" ]
             Draggable true
             OnDragEnd(fun _ -> dispatch DragLeft)
             OnDragStart(fun _ -> Dragging(name) |> dispatch) ]

let card dispatch name v foot =
    [ div [ Class "card" ] [ name
                             |> Option.map 
                                    (fun name -> 
                                    header [ Class "card-header" ] 
                                        [ p [ Class "card-header-title" ] 
                                              [ str name 
                                                |> draggable dispatch name ]
                                          
                                          a [ Href "#"
                                              Class "card-header-icon" ] 
                                              [ span [ Class "icon" ] 
                                                    [ i 
                                                          [ Class 
                                                                "fa fa-angle-down" ] 
                                                          [] ] ] ])
                             |> ofOption
                             
                             div [ Class "card-content" ] 
                                 [ div [ Class "content" ] [ v ] ]
                             foot
                             |> Option.map (footer [ Class "card-footer" ])
                             |> ofOption ]
      br [] ]
    |> ofList
let mkRows dragging hideAddButton (coreDispatch : Msg -> unit) icons row =
    let unparseEquation dragging operation envIcons dispatch =
        let rec unparseV envIcons (dispatch : GamePrimitive -> unit) =
            function 
            | Int(i) -> string i |> str
            | Float(f) -> sprintf "%.1f" f |> str
            | Dist(d) -> unparseDist (unparseV envIcons dispatch) d
            | NoValue -> str "--"
            | Str s -> b [] [ str s ]
            | Tuple(v, v2) -> 
                [ unparseV envIcons (fun gp -> Tuple(gp, v2) |> dispatch) v
                  str ","
                  unparseV envIcons (fun gp -> Tuple(v, gp) |> dispatch) v2 ]
                |> ofList
                |> paren
            | Check c -> unparseCheck (unparseV envIcons dispatch) c
            | ParamArray [] -> ofOption None
            | ParamArray [ Value(Str _); Value(NoValue) ] -> ofOption None
            | ParamArray [ Value(Str _); Var _ ] -> ofOption None
            | ParamArray [ Value(Str s); Value(v) ] -> 
                [ unparseV envIcons (fun s -> 
                      ParamArray [ Value s
                                   Value(v) ]
                      |> dispatch) (Str s)
                  unparseV envIcons (fun v -> 
                      ParamArray [ Value(Str s)
                                   Value(v) ]
                      |> dispatch) v ]
                |> List.map (List.singleton >> div [ ClassName "control" ])
                |> div [ ClassName "field is-grouped is-grouped-multiline" ]
            | ParamArray ops -> 
                ops
                |> Zipper.permute
                |> List.collect (function 
                       | Zipper.Empty -> []
                       | Zipper(l, a, r) -> 
                           [ unparseEq a envIcons (fun op' -> 
                                 Zipper(l, op', r)
                                 |> Zipper.toList
                                 |> ParamArray
                                 |> dispatch) ])
                |> List.reduce (fun a b -> 
                       [ a
                         str "; "
                         b ]
                       |> ofList)
                |> squareParen
        
        and unparseApp f a envIcons dispatch : Fable.Import.React.ReactElement =
            let (specialCall, joinStr) =
                match f with
                | Call Product -> None, str " * "
                | Call Division -> None, str " / "
                | Call Sub -> None, str " - "
                | Call Total -> None, str " + "
                | Call GreaterThan -> None, str " > "
                | Call Equals -> None, str " == "
                | Call NotEquals -> None, str " != "
                | Call LessThan -> None, str " < "
                | Call And -> None, str " && "
                | Call Or -> None, str " || "
                | Call Count -> Some(Call Repeat), str ", "
                | Call Repeat -> Some(Call Repeat), str " × "
                | Call Dice -> Some(Call Repeat), ofOption None
                | Call Max -> Some(Call Max), str ", "
                | Call Min -> Some(Call Min), str ", "
                | Call Median -> Some(Call Median), str ", "
                | Call Mean -> Some(Call Mean), str ", "
                | Call Mode -> Some(Call Mode), str ", "
                | Call Least -> Some(Call(Least)), str ", "
                | Call Largest -> Some(Call(Largest)), str ", "
                | c -> Some c, str "; "
            match a with
            | Value(ParamArray ops) -> 
                let call =
                    specialCall 
                    |> Option.map 
                           (fun f -> 
                           unparseEq f envIcons (fun op -> (op, a) |> dispatch))
                let ops' = ops |> Zipper.permute
                
                let param =
                    List.foldBack (function 
                        | Zipper.Empty -> id
                        | Zipper(l, a, r) -> 
                            fun acc -> 
                                let reset (e : Fable.Import.React.DragEvent) =
                                    e.target?className <- "has-text-centered"
                                    e.target?style?border <- "3px dotted black"
                                    e.target?style?``background-color`` <- ""
                                    e.target?style?width <- "18px"
                                    e.target?style?height <- "18px"
                                
                                let tail =
                                    let newValuePlaceholder =
                                        Option.map (fun dragging -> 
                                            [ span [] [ joinStr ]
                                              
                                              span 
                                                  [ OnDragOver
                                                        (fun e -> 
                                                        e.preventDefault()
                                                        e.dataTransfer.dropEffect <- "move"
                                                        e.target?className <- "has-text-centered"
                                                        e.target?style?border <- "3px dotted green"
                                                        e.target?style?``background-color`` <- "#0088CC"
                                                        e.target?style?width <- "24px"
                                                        e.target?style?height <- "24px")
                                                    OnDragLeave reset
                                                    OnDrop(fun e -> 
                                                        reset e
                                                        (f, 
                                                         List.append ops 
                                                             [ Var dragging ]
                                                         |> ParamArray
                                                         |> Value)
                                                        |> dispatch)
                                                    Class("has-text-centered")
                                                    
                                                    Style 
                                                        [ StrokeWidth 1.
                                                          Border "dotted"
                                                          Margin "3px"
                                                          Width "18px"
                                                          Height "18px"
                                                          Display "inline-block" ] ] 
                                                  [] ]
                                            |> ofList) dragging
                                    match acc with
                                    | [] -> Option.toList newValuePlaceholder
                                    | _ -> joinStr :: acc
                                
                                unparseEq a envIcons (fun op' -> 
                                    (f, 
                                     Zipper(l, op', r)
                                     |> Zipper.toList
                                     |> ParamArray
                                     |> Value)
                                    |> dispatch)
                                :: tail) ops' []
                    |> ofList
                tags [ call
                       |> Option.map (tag "is-primary")
                       |> ofOption
                       tag "is-success" (squareParen param) ]
            | Value(NoValue) -> 
                unparseEq f envIcons (fun op -> (op, a) |> dispatch) 
                |> tag "is-primary"
            | _ -> 
                let unparsedF =
                    unparseEq f envIcons (fun op -> (op, a) |> dispatch)
                let unparsedA =
                    unparseEq a envIcons (fun op -> (f, op) |> dispatch)
                tags [ tag "is-primary" unparsedF
                       tag "is-white" unparsedA ]
        
        and unparseChoice envIcons dispatch 
            (choices : (string * Operation) list) =
            choices
            |> Zipper.permute
            |> List.collect (function 
                   | Zipper.Empty -> []
                   | Zipper(l, a, r) -> 
                       [ str (fst a + ":= ")
                         br []
                         unparseEq (snd a) envIcons (fun op' -> 
                             Zipper(l, (fst a, op'), r)
                             |> Zipper.toList
                             |> dispatch) ])
            |> ofList
        
        and unparseC (func:Call) envIcons dispatch =
            match func with 
            | Product -> "Product "
            | Division -> "Division "
            | Total -> "Total "
            | Count -> "Count "
            | Repeat -> "Repeat "
            | Dice -> "Dice "
            | GreaterThan -> "GreaterThan "
            | Contains -> "Contains "
            | Equals -> "Equals "
            | NotEquals -> "NotEquals "
            | LessThan -> "LessThan "
            | ToDist -> "ToDist "
            | And -> "And "
            | Or -> "Or "
            | Max -> "Max "
            | Min -> "Min "
            | Sub -> "Sub "
            | Median -> "Median "
            | FMap -> "Map "
            | Mean -> "Mean "
            | Mode -> "Mode "
            | Least -> "Least "
            | Largest -> "Largest "
            |> string
            |> str
        
        and unparseEq op envIcons (dispatch : Operation -> unit) : Fable.Import.React.ReactElement =
            match op with
            | Call f -> unparseC f envIcons (Call >> dispatch)
            | Value(v) -> unparseV envIcons (Value >> dispatch) v
            | Var(v) -> 
                match envIcons |> Map.tryFind v with
                | Some icon -> b [] [ icon ]
                | None -> str v
            | Lam("unusedVariable", body) -> 
                unparseEq body (Map.remove "unusedVariable" envIcons) 
                    (fun op -> Lam("unusedVariable", op) |> dispatch)
            | WithLams((apps, lams), op) -> 
                let envIcons =
                    lams 
                    |> List.fold (fun state i -> Map.remove i state) envIcons
                let ev =
                    unparseEq op envIcons 
                        (fun op -> 
                        GameActions.Primitives.State.applyMany lams op apps 
                        |> dispatch)
                let headerItems = None
                
                let footerItems =
                    List.zip lams apps
                    |> Zipper.permute
                    |> List.collect 
                           (function //Empty -> lams)
                           | Zipper.Empty -> []
                           | Zipper(_, (a, None), _) -> 
                               let nameLabel = b [] [ str a ]
                               [ div 
                                     [ Class 
                                           "card-footer-item has-background-warning" ] 
                                     [ (match row with
                                        | _, ReadOnly _ -> nameLabel
                                        | _, ReadWrite _ -> 
                                            nameLabel 
                                            |> draggable coreDispatch a) ] ]
                           //GameActions.Primitives.State.applyMany (Zipper(l |> List.map fst, a, r |> List.map fst) |> Zipper.toList) op apps |> Zipper.toList) |> dispatch
                           | Zipper(l, (a, Some app), r) -> 
                               let nameLabel = b [] [ str (a + ": ") ]
                               [ div 
                                     [ Class 
                                           "card-footer-item has-background-warning" ] 
                                     [ (match row with
                                        | _, ReadOnly _ -> nameLabel
                                        | _, ReadWrite _ -> 
                                            nameLabel 
                                            |> draggable coreDispatch a)
                                       
                                       unparseEq app envIcons 
                                           (fun app' -> 
                                           GameActions.Primitives.State.applyMany 
                                               lams op 
                                               (Zipper
                                                    (l |> List.map snd, 
                                                     Some app', 
                                                     r |> List.map snd) 
                                                |> Zipper.toList) |> dispatch) ] ])
                
                let paramsCaption =
                    div [ Class "card-footer-item has-background-white" ] 
                        [ str "Params: " ]
                card coreDispatch headerItems ev 
                    (Some(paramsCaption :: footerItems))
            | Lam(x, body) -> 
                let envIcons = Map.remove "x" envIcons
                str (x + " => ") 
                :: [ unparseEq body envIcons (fun op -> Lam(x, op) |> dispatch) ]
                |> ofList
                |> tag "is-warning"
                |> List.singleton
                |> tags
            | Choice(name, choices) -> 
                [ str <| name + "one of: "
                  br []
                  
                  ul [] 
                      [ unparseChoice envIcons 
                            (fun ch -> Choice(name, ch) |> dispatch) choices ] ]
                |> ofList
            | GameActions.Primitives.State.IsDPlus(n, plus) -> 
                match n with
                | 6 -> 
                    [ d6icon
                      str (string (plus) + "+") ]
                    |> ofList
                | 3 -> 
                    [ d3icon
                      str (string (plus) + "+") ]
                    |> ofList
                | n -> string (plus) + "+ on D" + (string n) |> str
                |> tag "is-warning"
            | App(f, a) -> unparseApp f a envIcons (App >> dispatch)
            | Let(name, v, inner) -> 
                let ev =
                    unparseEq v envIcons 
                        (fun op -> Let(name, op, inner) |> dispatch)
                let einner =
                    unparseEq inner envIcons 
                        (fun op -> Let(name, v, op) |> dispatch)
                [ card coreDispatch (Some name) ev None
                  einner ]
                |> ofList
            | PropertyGet(s, op) -> 
                [ unparseEq op envIcons 
                      (fun op -> PropertyGet(s, op) |> dispatch)
                  str <| sprintf ".%s" s ]
                |> ofList
            | AsElseIfs(ifThens) -> 
                ifThens
                |> Zipper.permute
                |> List.map (function 
                       | Zipper.Empty -> ofOption None
                       | Zipper([], (Some ifExpr, thenExpr), _) as z -> //If
                           let ifPart =
                               unparseIf envIcons (fun op -> 
                                   Zipper.update (Some op, thenExpr) z
                                   |> Zipper.toList
                                   |> applyManyIfs
                                   |> dispatch) ifExpr
                           
                           let thenPart =
                               unparseThen envIcons (fun op -> 
                                   Zipper.update (Some ifExpr, op) z
                                   |> Zipper.toList
                                   |> applyManyIfs
                                   |> dispatch) thenExpr
                           
                           [ ifPart; thenPart ] |> tags
                       | Zipper(_, (Some ifExpr, thenExpr), _) as z -> //ElseIf
                           let ifPart =
                               unparseElseIf envIcons (fun op -> 
                                   Zipper.update (Some op, thenExpr) z
                                   |> Zipper.toList
                                   |> applyManyIfs
                                   |> dispatch) ifExpr
                           
                           let thenPart =
                               unparseThen envIcons (fun op -> 
                                   Zipper.update (Some ifExpr, op) z
                                   |> Zipper.toList
                                   |> applyManyIfs
                                   |> dispatch) thenExpr
                           
                           [ ifPart; thenPart ] |> tags
                       | Zipper(_, (None, thenExpr), _) as z -> //Last/else
                           let elsePart =
                               unparseElse envIcons (fun op -> 
                                   Zipper.update (None, op) z
                                   |> Zipper.toList
                                   |> applyManyIfs
                                   |> dispatch) thenExpr
                           [ elsePart ] |> tags)
                |> ofList
                |> tagGroup
            | IfThenElse(ifExpr, thenExpr, Some elseExpr) -> 
                [ unparseIf envIcons 
                      (fun op -> 
                      IfThenElse(op, thenExpr, Some elseExpr) |> dispatch) 
                      ifExpr
                  
                  unparseThen envIcons 
                      (fun op -> 
                      IfThenElse(ifExpr, op, Some elseExpr) |> dispatch) 
                      thenExpr
                  
                  unparseElse envIcons 
                      (fun op -> 
                      IfThenElse(ifExpr, thenExpr, Some op) |> dispatch) 
                      elseExpr ]
                |> tags
            | IfThenElse(ifExpr, thenExpr, None) -> 
                [ unparseIf envIcons 
                      (fun op -> IfThenElse(op, thenExpr, None) |> dispatch) 
                      ifExpr
                  
                  unparseThen envIcons 
                      (fun op -> IfThenElse(ifExpr, op, None) |> dispatch) 
                      thenExpr ]
                |> tags
        
        and unparseIf envIcons dispatch ifExpr =
            [ tag "is-info" (str "if")
              (unparseEq ifExpr envIcons dispatch) ]
            |> ofList
        
        and unparseThen envIcons dispatch thenExpr =
            [ tag "is-info" (str "then")
              (unparseEq thenExpr envIcons dispatch) ]
            |> ofList
        
        and unparseElse envIcons dispatch elseExpr =
            [ tag "is-info" (str "else")
              (unparseEq elseExpr envIcons dispatch) ]
            |> ofList
        
        and unparseElseIf envIcons dispatch elseExpr =
            [ tag "is-info" (str "elseif")
              (unparseEq elseExpr envIcons dispatch) ]
            |> ofList
        
        unparseEq operation envIcons dispatch
    
    let iconDisplay name icon =
        match icon with
        | HasIcon icon -> 
            Map.add name icon icons, draggable coreDispatch name icon
        | _ -> icons, draggable coreDispatch name (str name)
    
    match row with
    | _, ReadOnly(name, icon, gameAction, _) -> 
        let newIcons, iconDisplay = iconDisplay name icon
        tr [] 
            [ td [] 
                  [ (if hideAddButton then str ""
                     else 
                         a [ ClassName "button fa fa-pencil-square-o"
                             OnClick(fun _ -> EditRow(name) |> coreDispatch) ] 
                             [ str "Edit" ]) ]
              td [] [ iconDisplay ]
              
              td [ Style [ Position "relative" ] ] 
                  [ unparseEquation None gameAction icons ignore ] //dispatch)
                                                                   ], newIcons
    | _, ReadWrite(name, icon, op) -> 
        let newIcons, _ = iconDisplay name icon
        tr [] 
            [ td [] 
                  [ a [ ClassName "button fa fa-floppy-o"
                        OnClick(fun _ -> SaveOp(name) |> coreDispatch) ] 
                        [ str "Close" ] ]
              
              td [ ColSpan 3 ] 
                  [ div [ ClassName "field" ] [ label [ ClassName "label" ] 
                                                    [ str "Icon" ]
                                                input [ ClassName "input"
                                                        Type "text"
                                                        
                                                        Placeholder 
                                                            "FA Icon/Special/Text"
                                                        
                                                        DefaultValue
                                                            (icon |> function 
                                                             | Special s | Text(Some s) -> 
                                                                 s
                                                             | Icon s -> 
                                                                 s.Remove(0, 3)
                                                             | Text None -> "")
                                                        OnChange(fun ev -> 
                                                            !!ev.target?value
                                                            |> ChangeIcon
                                                            |> coreDispatch) ] ]
                    div [ ClassName "field" ] [ label [ Class "label" ] 
                                                    [ str "Name" ]
                                                input [ ClassName "input"
                                                        Type "text"
                                                        
                                                        Placeholder 
                                                            "Type the action name"
                                                        DefaultValue name
                                                        AutoFocus true
                                                        OnChange(fun ev -> 
                                                            !!ev.target?value
                                                            |> ChangeNewRowName
                                                            |> coreDispatch) ] ]
                    
                    div [ ClassName "field" ] 
                        [ label [ ClassName "label" ] [ str "Equation / Steps" ]
                          
                          unparseEquation dragging op icons 
                              (fun op -> Dragged(name, op) |> coreDispatch) ] ] ], 
        newIcons

let root model dispatch =
    let (tableRows, _) =
        List.mapFold (mkRows model.Dragging model.Editing dispatch) 
            Map.empty model.Functions
    table [ ClassName "table is-fullwidth  is-striped" ] [ thead [] 
                                                               [ tr [] 
                                                                     [ th [] 
                                                                           [ (if model.Editing then 
                                                                                  str 
                                                                                      ""
                                                                              else 
                                                                                  a 
                                                                                      [ ClassName 
                                                                                            "button fa fa-plus-circle"
                                                                                        
                                                                                        OnClick
                                                                                            (fun _ -> 
                                                                                            AddRow 
                                                                                            |> dispatch) ] 
                                                                                      [ str 
                                                                                            "Add" ]) ]
                                                                       
                                                                       th [] 
                                                                           [ str 
                                                                                 "Name" ]
                                                                       
                                                                       th [] 
                                                                           [ str 
                                                                                 "Equation / Steps" ] ] ]
                                                           tbody [] tableRows ]
