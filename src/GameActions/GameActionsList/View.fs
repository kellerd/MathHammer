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

open Microsoft.FSharp.Reflection

let inline toString (x:'a) = 
    let a = typeof<'a>
    match FSharpValue.GetUnionFields(x, a) with
    | case, _ -> case.Name

let inline  fromString<'a> (s:string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
    |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
    |_ -> None
let callList call = 
    toString call + " " |> str
    // div [Class "field"] 
    //     [
    //         // p [ Class "control has-icons-left"]
    //         //     [ span [Class "select" ] [
    //                 select [ DefaultValue (toString call)] 
    //                     [ option [ HTMLAttr.Value (toString Product)     ; Title "Product (...params...)"                ] [str (toString Product)    ] 
    //                       option [ HTMLAttr.Value (toString Division)    ; Title "Division (...params...)"               ] [str (toString Division)   ] 
    //                       option [ HTMLAttr.Value (toString Total)       ; Title "Total (...params...)"                  ] [str (toString Total)      ] 
    //                       option [ HTMLAttr.Value (toString Count)       ; Title "Count (Passes,Fails) in (...params...)"] [str (toString Count)      ] 
    //                       option [ HTMLAttr.Value (toString Repeat)      ; Title "Repeat (x,y)"                          ] [str (toString Repeat)     ] 
    //                       option [ HTMLAttr.Value (toString Dice)        ; Title "Die of "                               ] [str (toString Dice)       ] 
    //                       option [ HTMLAttr.Value (toString GreaterThan) ; Title "GreaterThan (x,y)"                     ] [str (toString GreaterThan)] 
    //                       option [ HTMLAttr.Value (toString Equals)      ; Title "Equals (x,y)"                          ] [str (toString Equals)     ] 
    //                       option [ HTMLAttr.Value (toString NotEquals)   ; Title "NotEquals (x,y)"                       ] [str (toString NotEquals)  ] 
    //                       option [ HTMLAttr.Value (toString LessThan)    ; Title "LessThan (x,y)"                        ] [str (toString LessThan)   ] 
    //                       option [ HTMLAttr.Value (toString And)         ; Title "And (x,y)"                             ] [str (toString And)        ] 
    //                       option [ HTMLAttr.Value (toString Or)          ; Title "Or (x,y)"                              ] [str (toString Or)         ] 
    //                       option [ HTMLAttr.Value (toString Max)         ; Title "Max (...params...)"                    ] [str (toString Max)        ] 
    //                       option [ HTMLAttr.Value (toString Min)         ; Title "Min (...params...)"                    ] [str (toString Min)        ] 
    //                       option [ HTMLAttr.Value (toString Sub)         ; Title "Sub (...params...)"                    ] [str (toString Sub)        ] 
    //                       option [ HTMLAttr.Value (toString Median)      ; Title "Median (...params...)"                 ] [str (toString Median)     ] 
    //                       option [ HTMLAttr.Value (toString Mean)        ; Title "Mean (...params...)"                   ] [str (toString Mean)       ] 
    //                       option [ HTMLAttr.Value (toString Mode)        ; Title "Mode (...params...)"                   ] [str (toString Mode)       ] 
    //                       option [ HTMLAttr.Value (toString Least)       ; Title "Least  (n, ...params...)"              ] [str (toString Least)      ] 
    //                       option [ HTMLAttr.Value (toString Largest)     ; Title "Largest  (n, ...params...)"            ] [str (toString Largest)    ] 
    //                     ] ]      
                //   span [Class "icon is-small is-left"] [ i [Class "fa fa-terminal"] [] ]
                // ]
        // ]                  

let (|HasIcon|_|)  = function 
   | Special "D6" -> d6icon |> Some
   | Special "D3" -> d3icon |> Some
   | Icon s       -> div [ClassName ("fa " + s)] [] |> Some
   | Text(Some s) -> strong [] [str s] |> Some
   | _            -> None      
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
                call  @  paren (param)
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
            callList func
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
    let draggable name item = 
        item 
        |> List.singleton 
        |> div [ Style [Cursor "move"]
                 Draggable true 
                 OnDragEnd (fun _ -> dispatch DragLeft)
                 OnDragStart (fun _ -> Dragging(name) |> dispatch)]
    let iconDisplay name icon = 
        match icon with 
        | HasIcon icon -> Map.add name icon icons, draggable name icon 
        | _ -> icons, draggable name (str name)
    match row with 
    | ReadOnly (name, icon, gameAction) -> 
        let newIcons,iconDisplay = iconDisplay name icon
        tr [] [
            td [] [(if hideAddButton then str "" 
                    else  a [ ClassName "button fa fa-pencil-square-o"; OnClick (fun _ -> EditRow(name) |> dispatch) ] [str "Edit"] )]
            td [] [iconDisplay]
            td [Style [Position "relative"]] [unparseEquation None gameAction ignore |> div [Class "columns" ]] //dispatch)
        ], newIcons
    | ReadWrite(name,icon,op) -> 
        let newIcons,_ = iconDisplay name icon
        tr [] [
            td [] [ a [ ClassName "button fa fa-floppy-o"
                        OnClick (fun _ -> SaveOp(name) |> dispatch)  ] [str "Close"] ]
            td [ColSpan 3.] [ 
                div [ClassName "field"] 
                    [
                        label [ClassName "label"] [str "Icon"]
                        input [ ClassName "input"
                                Type "text"
                                Placeholder "FA Icon/Special/Text"
                                DefaultValue (icon |> function Special s  | Text(Some s) -> s | Icon s -> s.Remove(0,3) | Text None -> "")
                                OnChange (fun ev -> !!ev.target?value |> ChangeIcon |> dispatch ) ]    
                    ]
                div [ClassName "field"] 
                    [
                        label [Class "label"] [str "Name"] 
                        input [ ClassName "input"
                                Type "text"
                                Placeholder "Type the action name"
                                DefaultValue name
                                AutoFocus true 
                                OnChange (fun ev -> !!ev.target?value |> ChangeNewRowName |> dispatch ) ]
                    ]      
                [div [ClassName "field"] 
                    [
                        label [ClassName "label"] [str "Equation / Steps"]
                        (unparseEquation dragging op (fun op -> Dragged(name,op) |> dispatch)) |> ofList
                    ]] |> div [Class "columns" ]
            ] ], newIcons
let root model dispatch =
    let (tableRows,_) = List.mapFold (mkRows model.Dragging model.Editing dispatch) Map.empty<_,_> model.Functions
    table [ClassName "table is-fullwidth  is-striped"] 
        [   thead [] [
                tr [] [
                    th [] [
                        (if model.Editing then str "" 
                        else a [ ClassName "button fa fa-plus-circle"; OnClick (fun _ -> AddRow |> dispatch) ] [str "Add"] )
                    ]        
                    th [] [ str "Name" ]
                    th [] [ str "Equation / Steps"] ] ] 
            tbody [] tableRows ]

    

      
