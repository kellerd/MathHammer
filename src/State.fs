module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types
open GameActions.Primitives.State

let pageParser : Parser<Page -> Page, Page> =
    oneOf [ map MathHammer (s "mathhammer")
            map GameActions (s "gameactions") ]

let urlUpdate (result : Option<Page>) model =
    match result with
    | None -> 
        console.error ("Error parsing url")
        model, Navigation.modifyUrl (toHash model.currentPage)
    | Some page -> { model with currentPage = page }, []
let init result =
    let (mathHammer, mathHammerCmd) = MathHammer.State.init()
    let (gameActions, gameActionsCmd) = GameActions.State.init()
    
    let (model, cmd) =
        urlUpdate result { currentPage = MathHammer
                           mathHammer = mathHammer
                           gameActions = gameActions }
    
    model, 
    Cmd.batch [ cmd
                Cmd.map MathHammerMsg mathHammerCmd
                Cmd.map GameActionsMsg gameActionsCmd ]

open GameActions.GameActionsList.Types

let mathHammerUpdate msg model =
    let (mathHammer, mathHammerCmd) = MathHammer.State.update msg model.mathHammer
    { model with mathHammer = mathHammer }, Cmd.map MathHammerMsg mathHammerCmd

let update msg model =
    match msg with
    | MathHammerMsg(MathHammer.Types.RebindEnvironment as msg) -> 
        let (choices, operations) =
            model.gameActions.Actions.Functions
            |> List.mapi (fun i -> 
                   function 
                   | choices, ReadWrite(str, _, op) -> 
                       let choice, normalized = normalize op
                       Map.mergeSets choice choices, (str, (i, normalized))
                   | choices, ReadOnly(str, _, _, normalized) -> choices, (str, (i, normalized)))
            |> List.unzip
        mathHammerUpdate msg { model with mathHammer =
                                              { model.mathHammer with GlobalOperations = operations
                                                                      Choices = List.reduceSafe Map.empty<_, _> Map.mergeSets choices } }
    | MathHammerMsg msg -> mathHammerUpdate msg model
    | GameActionsMsg(msg) -> 
        let (gameActions, gameActionsCmd) = GameActions.State.update msg model.gameActions
        
        let rebindCmds =
            match msg with
            | GameActions.Types.Msg.GameActionListMsg(SaveOp _) -> 
                Cmd.batch [ Cmd.map MathHammerMsg (Cmd.ofMsg (MathHammer.Types.RebindEnvironment))
                            Cmd.map GameActionsMsg gameActionsCmd ]
            | _ -> Cmd.map GameActionsMsg gameActionsCmd
        { model with gameActions = gameActions }, rebindCmds
