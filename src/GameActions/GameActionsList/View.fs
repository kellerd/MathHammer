module GameActions.GameActionsList.View
open Fable.Core.JsInterop
open Fable.Helpers.React
open Props
open GameActions.Primitives.Types
open GameActions.Primitives.View
open Types

let d6icon = 
    svg [ SVGAttr.Width 18
          SVGAttr.Height 18
          ViewBox "0 0 24 24"]
        [
              path [SVGAttr.Fill "#000000" 
                    D "M5,3H19A2,2 0 0,1 21,5V19A2,2 0 0,1 19,21H5A2,2 0 0,1 3,19V5A2,2 0 0,1 5,3M7,5A2,2 0 0,0 5,7A2,2 0 0,0 7,9A2,2 0 0,0 9,7A2,2 0 0,0 7,5M17,15A2,2 0 0,0 15,17A2,2 0 0,0 17,19A2,2 0 0,0 19,17A2,2 0 0,0 17,15M17,10A2,2 0 0,0 15,12A2,2 0 0,0 17,14A2,2 0 0,0 19,12A2,2 0 0,0 17,10M17,5A2,2 0 0,0 15,7A2,2 0 0,0 17,9A2,2 0 0,0 19,7A2,2 0 0,0 17,5M7,10A2,2 0 0,0 5,12A2,2 0 0,0 7,14A2,2 0 0,0 9,12A2,2 0 0,0 7,10M7,15A2,2 0 0,0 5,17A2,2 0 0,0 7,19A2,2 0 0,0 9,17A2,2 0 0,0 7,15Z"] []
        ]
let d3icon = 
    svg [ SVGAttr.Width 18
          SVGAttr.Height 18
          ViewBox "0 0 24 24"]
        [
              path [SVGAttr.Fill "#000000" 
                    D "M5,3H19A2,2 0 0,1 21,5V19A2,2 0 0,1 19,21H5A2,2 0 0,1 3,19V5A2,2 0 0,1 5,3M12,10A2,2 0 0,0 10,12A2,2 0 0,0 12,14A2,2 0 0,0 14,12A2,2 0 0,0 12,10M7,5A2,2 0 0,0 5,7A2,2 0 0,0 7,9A2,2 0 0,0 9,7A2,2 0 0,0 7,5M17,15A2,2 0 0,0 15,17A2,2 0 0,0 17,19A2,2 0 0,0 19,17A2,2 0 0,0 17,15Z"] []
        ]

open Microsoft.FSharp.Reflection
open Distribution.Example

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

let tags tags =
    div [ClassName "tags is-marginless is-medium has-addons "] tags
let tag tagClass item = 
    span [ ClassName ("tag is-marginless is-medium " + tagClass) ] [item]           
let tagGroup tags = 
    tags |> List.singleton |> div [Class "field is-grouped is-grouped-multiline"]  
let draggable dispatch name item  = 
    item 
    |> List.singleton 
    |> div [ Style [Cursor "move"]
             Draggable true 
             OnDragEnd (fun _ -> dispatch DragLeft)
             OnDragStart (fun _ -> Dragging(name) |> dispatch)]             
let card dispatch name v foot =
    [   div [ Class "card" ]
            [ name 
              |> Option.map (fun name -> 
                    header [ Class "card-header" ]
                           [ p [ Class "card-header-title" ] [ str name |> draggable dispatch name ]
                             a [ Href "#"
                                 Class "card-header-icon" ]
                               [ span [ Class "icon" ] 
                                      [ i [ Class "fa fa-angle-down" ] [] ] ] ])
               |> ofOption
              div [ Class "card-content" ] 
                  [ div [ Class "content" ] [ v ] ]
              foot
              |> Option.map (footer  [ Class "card-footer" ])          
              |> ofOption

            ]   
        br [] ]
    |> ofList

let rec (|AsElseIfs|_|)  = function 
    | IfThenElse(ifExpr,thenExpr, Some(AsElseIfs(ifThens))) ->  Some ((Some ifExpr, thenExpr) :: ifThens) 
    | IfThenElse(ifExpr,thenExpr, Some(elseEnd)) -> Some ([Some ifExpr, thenExpr; None, elseEnd] )
    | IfThenElse(ifExpr,thenExpr, None) -> Some [Some ifExpr, thenExpr]
    | _ -> None
let applyManyIfs ifThens = 
    let rec halp ifThens = 
        match ifThens with 
        | [] -> None 
        | [None,elseExpr] -> Some elseExpr 
        | (Some ifExpr,thenExpr)::xs -> IfThenElse(ifExpr,thenExpr, halp xs) |> Some 
        | _ -> None
    match halp ifThens with 
    | Some v -> v
    | None -> Value(NoValue)


// let x1 = IfThenElse(Value(Int(6)), Value(Int(7)), None) 
// let x2 = IfThenElse(Value(Int(6)), Value(Int(7)), Some(IfThenElse(Value(Int(8)), Value(Int(9)), Some(Value(Int(10)))))) 
// let x3 = IfThenElse(Value(Int(6)), Value(Int(7)), Some(IfThenElse(Value(Int(8)), Value(Int(9)), None))) 
// let x4 = IfThenElse(Value(Int(6)), Value(Int(7)), Some(IfThenElse(Value(Int(8)), Value(Int(9)), Some(IfThenElse(Value(Int(10)), Value(Int(11)), Some(IfThenElse(Value(Int(12)), Value(Int(13)), Some(Value(Int(14))))))))))
// let x5 = IfThenElse(Value(Int(6)), Value(Int(7)), Some(IfThenElse(Value(Int(8)), Value(Int(9)), Some(IfThenElse(Value(Int(10)), Value(Int(11)), Some(IfThenElse(Value(Int(12)), Value(Int(13)), None))))))) 

// x1 |> (|AsElseIfs|_|) |> Option.bind applyManyIfs = Some x1
// x2 |> (|AsElseIfs|_|) |> Option.bind applyManyIfs = Some x2
// x3 |> (|AsElseIfs|_|) |> Option.bind applyManyIfs = Some x3
// x4 |> (|AsElseIfs|_|) |> Option.bind applyManyIfs = Some x4
// x5 |> (|AsElseIfs|_|) |> Option.bind applyManyIfs = Some x5
// NoValue |> Value  |> (|AsElseIfs|_|) |> Option.bind applyManyIfs = None

// type GamePrimitive =
//     | Check of Check.Check<GamePrimitive>
//     | ParamArray of Operation list
//     | Tuple of GamePrimitive * GamePrimitive
//     | Dist of Distribution.Distribution<GamePrimitive>
// and Operation = 
//     | PropertyGet of string * Operation
//     | Value of GamePrimitive
//     | App of f:Operation * value:Operation
//     | Lam of param:string * body:Operation
//     | Let of string * value:Operation * body:Operation
//     | IfThenElse of ifExpr:Operation * thenExpr:Operation * elseExpr:Operation option
//     | Choice of name : string * choices:(string * Operation) list       
let mkRows dragging hideAddButton (coreDispatch:Msg->unit) icons row = 
    let unparseEquation dragging operation dispatch = 
        let rec unparseV (dispatch:GamePrimitive->unit)  = function
                | Int(i) -> string i |> str
                | Float(f) -> sprintf "%.1f" f |> str
                | Dist(d) -> unparseDist (unparseV dispatch) d
                | NoValue -> str "--"
                | Str s -> b  [] [str s]
                | Tuple(v,v2) -> [ unparseV (fun gp -> Tuple(gp,v2) |> dispatch) v 
                                   str ","
                                   unparseV (fun gp -> Tuple(v,gp) |> dispatch) v2 ] |> ofList |> paren
                | Check c -> unparseCheck (unparseV dispatch) c
                | ParamArray [] ->  ofOption None
                | ParamArray [Value (Str _); Value(NoValue)] ->  ofOption None
                | ParamArray [Value (Str _); Var _] ->   ofOption None
                | ParamArray [Value (Str s); Value(v)] ->   
                    [ unparseV (fun s -> ParamArray [Value s;       Value(v)] |> dispatch ) (Str s)
                      unparseV (fun v -> ParamArray [Value (Str s); Value(v)] |> dispatch ) v ]
                    |> List.map (List.singleton >> div [ClassName "control"])
                    |> div [ClassName "field is-grouped is-grouped-multiline"]
                | ParamArray ops -> 
                    ops 
                    |> Zipper.permute 
                    |> List.collect(function 
                                    | Empty -> [] 
                                    | Zipper(l,a,r) -> [unparseEq a (fun op' -> Zipper(l,op',r) |> Zipper.toList |> ParamArray |> dispatch)])
                    |> List.reduce(fun a b -> [a; str "; "; b] |> ofList)
                    |> squareParen
        and unparseApp f a dispatch : Fable.Import.React.ReactElement = 
            let (specialCall, joinStr) = 
                match f  with 
                | Call Product      -> None, str " * "
                | Call Division     -> None, str " / "
                | Call Sub          -> None, str " - "
                | Call Total        -> None, str " + " 
                | Call GreaterThan  -> None, str ">"
                | Call Equals       -> None, str "=="
                | Call NotEquals    -> None, str "!="
                | Call LessThan     -> None, str "<"
                | Call And          -> None, str " && "
                | Call Or           -> None, str " || "
                | Call Count        -> Some (Call Repeat), str ", "
                | Call Repeat       -> Some (Call Repeat), str " × "
                | Call Dice         -> Some (Call Repeat), ofOption None
                | Call Max          -> Some(Call Max        ), str ", "
                | Call Min          -> Some(Call Min        ), str ", "
                | Call Median       -> Some(Call Median     ), str ", "
                | Call Mean         -> Some(Call Mean       ), str ", "
                | Call Mode         -> Some(Call Mode       ), str ", "
                | Call Least        -> Some(Call (Least    )), str ", "
                | Call Largest      -> Some(Call (Largest  )), str ", "            
                | c                 -> Some c, str "; "
            match a with 
            | Value(ParamArray ops) -> 
                let call = specialCall |> Option.map (fun f -> unparseEq f (fun op -> (op,a) |> dispatch))
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
                                        unparseEq a (fun op' -> (f,Zipper(l,op',r) |> Zipper.toList |> ParamArray |> Value) |> dispatch)::tail ) ops' [] 
                    |> ofList                                                    
                tags [ call |> Option.map (tag "is-primary" ) |> ofOption
                       tag "is-success" (squareParen param) ]               
            | Value(NoValue) -> 
                unparseEq f (fun op -> (op,a) |> dispatch)
                |> tag "is-primary"                 
            | _ -> 
                let unparsedF = unparseEq f (fun op -> (op,a) |> dispatch)
                let unparsedA = unparseEq a (fun op -> (f,op) |> dispatch)
                tags [ tag "is-primary" unparsedF
                       tag "is-white"   unparsedA]
        and unparseChoice dispatch (choices:(string*Operation) list) =
            choices 
            |> Zipper.permute  
            |> List.collect(function 
                            | Empty -> [] 
                            | Zipper(l,a,r) -> 
                                     [str (fst a + ":= ");
                                     br [];
                                     unparseEq (snd a) (fun op' -> Zipper(l,(fst a,op'),r) |> Zipper.toList |> dispatch)])  
            |> ofList
        and unparseC dispatch func = 
            callList func
        and unparseEq op (dispatch:Operation->unit) : Fable.Import.React.ReactElement =                      
            match op with 
            | Call f -> unparseC (Call >> dispatch) f
            | Value(v)-> unparseV (Value >> dispatch) v
            | Var (v) -> 
                match icons |> Map.tryFind v with 
                | Some icon -> b [] [icon]
                | None -> str v
            | Lam("unusedVariable",body) -> unparseEq body (fun op -> Lam("unusedVariable", op) |> dispatch)
            | WithLams ((apps,lams), op) ->
                let ev = unparseEq op (fun op -> GameActions.Primitives.State.applyMany lams op apps |> dispatch)
                let headerItems = None
                let footerItems = 
                    List.zip lams apps 
                    |> Zipper.permute
                    |> List.collect(function //Empty -> lams)
                                    | Empty -> []
                                    | Zipper(l,(a,   None), r) -> 
                                        let nameLabel = b [] [str a]
                                        [ 
                                            div [ Class "card-footer-item has-background-warning" ] 
                                                [ (match row with |ReadOnly _ -> nameLabel | ReadWrite _ -> nameLabel |> draggable coreDispatch a) ]
                                                //GameActions.Primitives.State.applyMany (Zipper(l |> List.map fst, a, r |> List.map fst) |> Zipper.toList) op apps |> Zipper.toList) |> dispatch
                                        ]
                                    | Zipper(l,(a, Some app), r) -> 
                                        let nameLabel = b [] [str (a + ": ")]
                                        [ 
                                            div [ Class "card-footer-item has-background-warning" ] 
                                                [ 
                                                  (match row with | ReadOnly _ -> nameLabel | ReadWrite _ -> nameLabel |> draggable coreDispatch a)
                                                  unparseEq app (fun app' -> GameActions.Primitives.State.applyMany lams op (Zipper(l |> List.map snd, Some app', r |> List.map snd) |> Zipper.toList) |> dispatch) ]
                                        ])  
                    |> Some                            
                card coreDispatch headerItems ev footerItems
            | Lam(x,body) -> 
                str (x + " => ") :: [unparseEq body (fun op -> Lam(x, op) |> dispatch)] |> ofList
                |> tag "is-warning"
                |> List.singleton 
                |> tags
            | Choice(name, choices) ->  
                [ str <| name + "one of: "
                  br []
                  ul [] [unparseChoice (fun ch -> Choice(name, ch) |> dispatch) choices]  ]
                |> ofList
            | GameActions.Primitives.State.IsDPlus(n,plus) ->  
                match n with 
                | 6 -> string (plus) + "+" |> str
                | n -> string (plus) + "+ on D" + (string n) |> str    
                |> tag "is-warning"          
            | App(f,a) -> unparseApp f a (App >> dispatch)
            | Let(name, v, inner) ->  
                let ev = unparseEq v (fun op -> Let(name, op, inner) |> dispatch)
                let einner = unparseEq inner (fun op -> Let(name, v, op) |> dispatch)
                [ card coreDispatch (Some name) ev None
                  einner ]
                |> ofList
                // [ str ("let " + name + " = ") 
                //   unparseEq v (fun op -> Let(name, op, inner) |> dispatch)
                //   br [] 
                //   unparseEq inner (fun op -> Let(name, v, op) |> dispatch) ]
                // |> ofList
            | PropertyGet(s,op) -> [unparseEq op (fun op -> PropertyGet(s,op) |> dispatch ); str <| sprintf ".%s" s] |> ofList
            | AsElseIfs(ifThens) ->    
                ifThens
                |> Zipper.permute
                |> List.map (function 
                    | Empty -> ofOption None
                    | Zipper([],(Some ifExpr, thenExpr),_ ) as z -> //If
                        let ifPart = unparseIf (fun op -> Zipper.update (Some op, thenExpr) z |> Zipper.toList |> applyManyIfs |> dispatch ) ifExpr
                        let thenPart = unparseThen (fun op -> Zipper.update (Some ifExpr, op) z  |> Zipper.toList |> applyManyIfs |> dispatch) thenExpr
                        [ifPart; thenPart] |> tags |> tagGroup
                    | Zipper(_ ,(Some ifExpr, thenExpr),_ ) as z -> //ElseIf
                        let ifPart = unparseElseIf (fun op -> Zipper.update (Some op, thenExpr) z |> Zipper.toList |> applyManyIfs |> dispatch ) ifExpr
                        let thenPart = unparseThen (fun op -> Zipper.update (Some ifExpr, op) z  |> Zipper.toList |> applyManyIfs |> dispatch) thenExpr
                        [ifPart; thenPart] |> tags
                    | Zipper(_, (None, thenExpr), _) as z -> //Last/else
                        let elsePart = unparseElse (fun op -> Zipper.update (None, op) z  |> Zipper.toList |> applyManyIfs |> dispatch) thenExpr
                        [elsePart] |> tags
                ) |> ofList
            | IfThenElse(ifExpr, thenExpr, Some elseExpr) ->
                [ unparseIf   (fun op -> IfThenElse(op,     thenExpr, Some elseExpr) |> dispatch ) ifExpr 
                  unparseThen (fun op -> IfThenElse(ifExpr, op,       Some elseExpr) |> dispatch ) thenExpr 
                  unparseElse (fun op -> IfThenElse(ifExpr, thenExpr, Some op)       |> dispatch ) elseExpr ] 
                |> tags
            | IfThenElse(ifExpr, thenExpr, None) ->
                [ unparseIf   (fun op -> IfThenElse(op,     thenExpr, None) |> dispatch ) ifExpr 
                  unparseThen (fun op -> IfThenElse(ifExpr, op,       None) |> dispatch ) thenExpr ] 
                |> tags
        and unparseIf     dispatch ifExpr    = [tag "is-info" (str "if");     (unparseEq ifExpr dispatch)  ] |> ofList
        and unparseThen   dispatch thenExpr  = [tag "is-info" (str "then");   (unparseEq thenExpr dispatch)] |> ofList
        and unparseElse   dispatch elseExpr  = [tag "is-info" (str "else");   (unparseEq elseExpr dispatch)] |> ofList
        and unparseElseIf dispatch elseExpr  = [tag "is-info" (str "elseif"); (unparseEq elseExpr dispatch)] |> ofList
        unparseEq operation dispatch
    let iconDisplay name icon = 
        match icon with 
        | HasIcon icon -> Map.add name icon icons, draggable coreDispatch name icon 
        | _ -> icons, draggable coreDispatch name (str name)
    match row with 
    | ReadOnly (name, icon, gameAction) -> 
        let newIcons,iconDisplay = iconDisplay name icon
        tr [] [
            td [] [(if hideAddButton then str "" 
                    else  a [ ClassName "button fa fa-pencil-square-o"; OnClick (fun _ -> EditRow(name) |> coreDispatch) ] [str "Edit"] )]
            td [] [iconDisplay]
            td [Style [Position "relative"]] [unparseEquation None gameAction ignore] //dispatch)
        ], newIcons
    | ReadWrite(name,icon,op) -> 
        let newIcons,_ = iconDisplay name icon
        tr [] [
            td [] [ a [ ClassName "button fa fa-floppy-o"
                        OnClick (fun _ -> SaveOp(name) |> coreDispatch)  ] [str "Close"] ]
            td [ColSpan 3.] [ 
                div [ClassName "field"] 
                    [
                        label [ClassName "label"] [str "Icon"]
                        input [ ClassName "input"
                                Type "text"
                                Placeholder "FA Icon/Special/Text"
                                DefaultValue (icon |> function Special s  | Text(Some s) -> s | Icon s -> s.Remove(0,3) | Text None -> "")
                                OnChange (fun ev -> !!ev.target?value |> ChangeIcon |> coreDispatch ) ]    
                    ]
                div [ClassName "field"] 
                    [
                        label [Class "label"] [str "Name"] 
                        input [ ClassName "input"
                                Type "text"
                                Placeholder "Type the action name"
                                DefaultValue name
                                AutoFocus true 
                                OnChange (fun ev -> !!ev.target?value |> ChangeNewRowName |> coreDispatch ) ]
                    ]      
                div [ClassName "field"] 
                    [
                        label [ClassName "label"] [str "Equation / Steps"]
                        unparseEquation dragging op (fun op -> Dragged(name,op) |> coreDispatch)
                    ]
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

    

      
