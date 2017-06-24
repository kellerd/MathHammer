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
    map About (s "about")
    map Counter (s "counter")
    map Home (s "home")
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
  let (counter, counterCmd) = Counter.State.init()
  let (home, homeCmd) = Home.State.init()
  let (mathHammer, mathHammerCmd) = MathHammer.State.init()
  let (gameActions, gameActionsCmd) = GameActions.State.init()
  let attackers = [initMeq "Marine"; initMeq "Captain"] |> List.map(fun (x,_) -> x.name,x) |> Map.ofList
  let defenders = ['a'..'z'] |> List.map(fun c -> c.ToString(),initGeq (c.ToString())|> fst)  |> Map.ofList

  let (model, cmd) =
    urlUpdate result
      { currentPage = Home
        counter = counter
        home = home
        mathHammer = { mathHammer with Attacker = {mathHammer.Attacker with Models = attackers}
                                       Defender = {mathHammer.Defender with Models = defenders} } 
        gameActions = gameActions                                }
  model, Cmd.batch [ cmd
                     Cmd.map CounterMsg counterCmd
                     Cmd.map HomeMsg homeCmd
                     Cmd.map MathHammerMsg mathHammerCmd
                     Cmd.map GameActionsMsg gameActionsCmd ]

let update msg model =
  match msg with
  | CounterMsg msg ->
      let (counter, counterCmd) = Counter.State.update msg model.counter
      { model with counter = counter }, Cmd.map CounterMsg counterCmd
  | HomeMsg msg ->
      let (home, homeCmd) = Home.State.update msg model.home
      { model with home = home }, Cmd.map HomeMsg homeCmd
  | MathHammerMsg msg ->
      let (mathHammer, mathHammerCmd) = MathHammer.State.update msg model.mathHammer
      { model with mathHammer = mathHammer }, Cmd.map MathHammerMsg mathHammerCmd
  | GameActionsMsg(msg) ->
      let (gameActions, gameActionsCmd) = GameActions.State.update msg model.gameActions
      { model with gameActions = gameActions }, Cmd.map GameActionsMsg gameActionsCmd
