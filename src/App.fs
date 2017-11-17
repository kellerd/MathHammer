module App.View

open Elmish
open Elmish.Browser.Navigation
open Browser.UrlParser
open Fable.Core
open JsInterop
open Types
open State
open Global

importAll "../sass/main.sass"

open Fable.Helpers.React
open Props

let menuItem label page currentPage =
    li
      [ ]
      [ a
          [ classList [ "is-active", page = currentPage ]
            Href (toHash page) ]
          [ str label ] ]

let menu currentPage =
  aside
    [ ClassName "menu" ]
    [ p
        [ ClassName "menu-label" ]
        [ str "General" ]
      ul
        [ ClassName "menu-list" ]
        [ menuItem "MathHammer" MathHammer currentPage
          menuItem "Game Actions" GameActions currentPage ] ]

let root model dispatch =

  let pageHtml =
    function
    | MathHammer -> MathHammer.View.root model.mathHammer (MathHammerMsg >> dispatch)
    | GameActions -> GameActions.View.root model.gameActions (GameActionsMsg >> dispatch)

  div
    []
    [ div
        [ ClassName "navbar-bg" ]
        [ div
            [ ClassName "container" ]
            [ Navbar.View.root ] ]
      div
        [ ClassName "section" ]
        [ div
            [ ClassName "container" ]
            [ div
                [ ClassName "columns" ]
                [ div
                    [ ClassName "column is-3" ]
                    [ menu model.currentPage ]
                  div
                    [ ClassName "column" ]
                    [ pageHtml model.currentPage ] ] ] ] ]

open Elmish.React
//open Elmish.Debug
// App
Program.mkProgram init update root
|> Program.toNavigable (parseHash pageParser) urlUpdate
|> Program.withReact "elmish-app"
//-:cnd
#if DEBUG
|> Program.withConsoleTrace
//|> Program.withDebuggerAt (Debugger.Remote("localhost",8097))
#endif
//+:cnd
|> Program.run
