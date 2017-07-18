module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types
open MathHammer.Models.State

let pageParser: Parser<Page->Page,Page> =
  oneOf [
    map MathHammer (s "mathhammer")
    map GameActions (s "gameactions")
  ]

let urlUpdate (result: Option<Page>) model =
  match result with
  | None ->
    console.error("Error parsing url")
    model,Navigation.modifyUrl (toHash model.currentPage)
  | Some page ->
      { model with currentPage = page }, []
open GameActions.Primitives.Types
let init result =
  let (mathHammer, mathHammerCmd) = MathHammer.State.init()
  let (gameActions, gameActionsCmd) = GameActions.State.init()
  let mapScale scope scale = 
    List.map (fun x -> x scope)
    >> List.mapi 
        (fun i (x : MathHammer.Models.Types.Model,_) -> 
          x.Name, {x with Attributes = 
                            x.Attributes
                            |> Map.map (fun k -> function i,Let(env,_,_) when k = "A" -> i,Let(env, "A" , Value (Int(i + 3))) | i,x -> i,x)
                          Scale = scale} )
    >> Map.ofList


  let attackers = [initMeq "Marine"; initMeq "Captain" ] 
                  |> mapScale GameActions.Primitives.Types.Attacker mathHammer.Attacker.Scale
  let defenders = ['a'..'z'] |> List.map (string >> initGeq)  |> mapScale GameActions.Primitives.Types.Defender mathHammer.Defender.Scale 

  let (model, cmd) =
    urlUpdate result
      { currentPage = MathHammer
        mathHammer = { mathHammer with Attacker = {mathHammer.Attacker with Models = attackers}
                                       Defender = {mathHammer.Defender with Models = defenders} } 
        gameActions = gameActions                                }
  model, Cmd.batch [ cmd
                     Cmd.map MathHammerMsg mathHammerCmd
                     Cmd.map GameActionsMsg gameActionsCmd ]

open GameActions.GameActionsList.Types

let mathHammerUpdate msg model =
  let (mathHammer, mathHammerCmd) = MathHammer.State.update msg model.mathHammer
  { model with mathHammer = mathHammer }, Cmd.map MathHammerMsg mathHammerCmd

let update msg model =
  match msg with
  | MathHammerMsg (MathHammer.Types.RebindEnvironment as msg) ->
    let operations = model.gameActions.Actions |> fst |> List.mapi (fun i -> function ReadWrite(str,op) -> str,(i,op) | ReadOnly (str,op) -> str,(i,op)) |> Map.ofList
    mathHammerUpdate msg {model with mathHammer = {model.mathHammer with GlobalOperations = operations }}
  | MathHammerMsg msg ->
      mathHammerUpdate msg model
  | GameActionsMsg(msg) ->
      let (gameActions, gameActionsCmd) = GameActions.State.update msg model.gameActions
      let rebindCmd = Cmd.ofMsg (MathHammer.Types.RebindEnvironment)
      { model with gameActions = gameActions }, Cmd.batch [ Cmd.map MathHammerMsg rebindCmd
                                                            Cmd.map GameActionsMsg gameActionsCmd]
