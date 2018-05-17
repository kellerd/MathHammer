module GameActions.State

open Elmish
open Types

let init() : Model * Cmd<Msg> =
    let (gameActionsList, gameActionsListCmd) = GameActionsList.State.init()
    { Actions = gameActionsList }, Cmd.map GameActionListMsg gameActionsListCmd

let update msg model : Model * Cmd<Msg> =
    match msg with
    | GameActionListMsg(msg) -> 
        let (gameActionsList, gameActionsListCmd) = GameActionsList.State.update msg model.Actions
        { model with Actions = gameActionsList }, Cmd.map GameActionListMsg gameActionsListCmd
