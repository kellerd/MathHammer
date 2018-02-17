module GameActions.View

open Types

let root model dispatch =
    GameActions.GameActionsList.View.root model.Actions (GameActionListMsg >> dispatch )
