module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types
open MathHammer.Models.State
open GameActions.Primitives.State

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
let range = vInt

let phaseActions = 
    choose "Phase" 
        [
            "Assault", 
                nestOps 
                    [ get "Assault Range"   <*> get "M" 
                                            <*> get "Charge Range"
                                            >>= "Assault Range"
                      get "To Hit"          <*> get "WS" 
                                            <*> get "A" 
                                            >>= "Hit Results"
                     // (get "Strength vs Toughness Table" <*> get "S" <*> get "Defender") >>= "Wound Results"                  
                      get "To Wound"        <*> get "Hit Results"
                                            <*> (get "Strength vs Toughness Table" <*> get "Defender"  <*> get "S")
                                            >>= "Wound Results"
                      get "App 2 Test"      >>= "App2"                                       
                      get "Armour Save"     <*> get "Defender" 
                                            <*> get "Wound Results"
                                            >>= "Unsaved Wounds" ] 
                                                          <| opList [ labelVar "Charge Range"
                                                                      labelVar "Assault Range"
                                                                      labelVar "Hit Results"
                                                                      labelVar "Wound Results"
                                                                      labelVar "App2"
                                                                      labelVar "Unsaved Wounds" ] 
            "Shooting", (labelVar "Shooting Range")
            "Psychic", (labelVar "Psychic Test")
        ] >>= "Actions"


let dPhaseActions = 
    choose "Phase" 
        [
            "Assault", (choose "Weapon" ["Bolter", range 24; "Melta", range 12] >>= "Weapon Range") (labelVar "Shooting Range")
            "Psychic", (labelVar "Deny Test")
        ] >>= "Actions"  
let allPropsa = 
    opList 
        [ labelVar "M"
          labelVar "WS"
          labelVar "BS"
          labelVar "S"
          labelVar "T"
          labelVar "W"
          labelVar "A"
          labelVar "Ld"
          labelVar "Sv"
          labelVar "InvSv"
          labelVar "D6Test"
          labelVar "D3Test"
          labelProp "Actions" "Charge Range"
          labelProp "Actions" "Assault Range"
          labelProp "Actions" "Hit Results"
          labelProp "Actions" "Wound Results"
          labelProp "Actions" "App2"
          labelProp "Actions" "Unsaved Wounds"
          labelProp "Actions" "Psychic Test"
          labelProp "Actions" "Shooting Range"  ]
let allPropsd = 
    opList 
        [ labelVar "M"
          labelVar "WS"
          labelVar "BS"
          labelVar "S"
          labelVar "T"
          labelVar "W"
          labelVar "A"
          labelVar "Ld"
          labelVar "Sv"
          labelVar "InvSv"
          labelVar "D6Test"
          labelVar "D3Test"
          labelProp "Actions" "Shooting Range" 
          labelProp "Actions" "Deny Test" ]
let init result =
    let (mathHammer, mathHammerCmd) = MathHammer.State.init()
    let (gameActions, gameActionsCmd) = GameActions.State.init()

    // attackerStats
    let body = 
        nestOps [phaseActions] allPropsa
    let defbody = 
        nestOps [dPhaseActions] allPropsd
    let stats = ["M";"WS";"BS";"S";"T";"W";"A";"Ld";"Sv";"InvSv"]  
    let attacker = createArgs stats body
    let defender =  createArgs stats defbody

    let attackers = [initMeq "M" attacker
                     initMeq "M2" attacker
                     initMeq "M3" attacker
                     initMeq "M4" attacker
                     initMeq "M5" attacker
                     initMeq "M6" attacker
                     initMeq "M8" attacker
                     initMeq "M12" attacker
                     initMeq "M13" attacker
                     initMeq "M14" attacker
                     initMeq "M15" attacker
                     initMeq "M16" attacker
                     initMeq "M22" attacker
                     initMeq "M23" attacker
                     initMeq "M24" attacker
                     initMeq "M25" attacker
                     initMeq "M26" attacker
                     initMeq "M32" attacker
                     initMeq "M33" attacker
                    //  initMeq "Marine34" attacker
                    //  initMeq "Marine35" attacker
                    //  initMeq "Marine36" attacker
                     initSgt "Captain" attacker ] 
                    |> List.map (fst)
                    |> Map.ofList
    let defenders = ['A'..'z'] 
                    |> List.map (fun c -> initGeq (string c) defender |> fst)
                    |> Map.ofList

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
        let operations = model.gameActions.Actions.Functions |> List.mapi (fun i -> function ReadWrite(str,_,op) -> str,(i,op) | ReadOnly (str,_,op) -> str,(i,op)) 
        mathHammerUpdate msg {model with mathHammer = {model.mathHammer with GlobalOperations = operations }}
    | MathHammerMsg msg ->
        mathHammerUpdate msg model
    | GameActionsMsg(msg) ->
        let (gameActions, gameActionsCmd) = GameActions.State.update msg model.gameActions
        let rebindCmds = 
            match msg with 
            | GameActions.Types.Msg.GameActionListMsg(SaveOp _) -> 
               Cmd.batch
                  [ Cmd.map MathHammerMsg (Cmd.ofMsg (MathHammer.Types.RebindEnvironment)) 
                    Cmd.map GameActionsMsg gameActionsCmd ]
            | _ -> Cmd.map GameActionsMsg gameActionsCmd                             
        { model with gameActions = gameActions }, rebindCmds
