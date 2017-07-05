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

let init result =
  let (mathHammer, mathHammerCmd) = MathHammer.State.init()
  let (gameActions, gameActionsCmd) = GameActions.State.init()
  let attackers = [initMeq "Marine" GameActions.Primitives.Types.Attacker; initMeq "Captain" GameActions.Primitives.Types.Attacker] |> List.map(fun (x,_) -> x.Name,{x with Scale = mathHammer.Attacker.Scale}) |> Map.ofList
  let defenders = ['a'..'z'] |> List.map(fun c -> c.ToString(), { (fst <| initGeq (c.ToString()) GameActions.Primitives.Types.Defender) with Scale = mathHammer.Defender.Scale})  |> Map.ofList

  let (model, cmd) =
    urlUpdate result
      { currentPage = MathHammer
        mathHammer = { mathHammer with Attacker = {mathHammer.Attacker with Models = attackers}
                                       Defender = {mathHammer.Defender with Models = defenders} } 
        gameActions = gameActions                                }
  model, Cmd.batch [ cmd
                     Cmd.map MathHammerMsg mathHammerCmd
                     Cmd.map GameActionsMsg gameActionsCmd ]

let update msg model =
  match msg with
  | MathHammerMsg msg ->
      let (mathHammer, mathHammerCmd) = MathHammer.State.update msg model.mathHammer
      { model with mathHammer = mathHammer }, Cmd.map MathHammerMsg mathHammerCmd
  | GameActionsMsg(msg) ->
      let (gameActions, gameActionsCmd) = GameActions.State.update msg model.gameActions
      { model with gameActions = gameActions }, Cmd.map GameActionsMsg gameActionsCmd
