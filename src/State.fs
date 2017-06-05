module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types
module Dudes = MathHammer.Models.State

let pageParser: Parser<Page->Page,Page> =
  oneOf [
    map About (s "about")
    map Counter (s "counter")
    map Home (s "home")
    map MathHammer (s "mathhammer")
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
  let (dude,_) = Dudes.init()
  let attackers = [{dude with name = "Marine";}; {dude with name = "Captain"}] |> List.map(fun x -> x.name,x) |> Map.ofList
  let defenders = ['a'..'z'] |> List.map(fun c -> c.ToString(),{dude with name = c.ToString()})  |> Map.ofList

  let (model, cmd) =
    urlUpdate result
      { currentPage = Home
        counter = counter
        home = home
        mathHammer = { mathHammer with Attacker = {mathHammer.Attacker with Models = attackers}
                                       Defender = {mathHammer.Defender with Models = defenders} }  }
  model, Cmd.batch [ cmd
                     Cmd.map CounterMsg counterCmd
                     Cmd.map HomeMsg homeCmd
                     Cmd.map MathHammerMsg mathHammerCmd ]

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
