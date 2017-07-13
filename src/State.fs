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
                            |> List.map (function (k,Let(env,_,_)) when k = "A" -> "A", (Let(env, "A" , Value (Int(i + 3)))) | x -> x)
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

let update msg model =
  match msg with
  | MathHammerMsg msg ->
      let (mathHammer, mathHammerCmd) = MathHammer.State.update msg model.mathHammer
      { model with mathHammer = mathHammer }, Cmd.map MathHammerMsg mathHammerCmd
  | GameActionsMsg(msg) ->
      let (gameActions, gameActionsCmd) = GameActions.State.update msg model.gameActions
      { model with gameActions = gameActions }, Cmd.map GameActionsMsg gameActionsCmd
