module MathHammer.State

open Elmish
open Types
open GameActions.Primitives.Types
let attackerMap msg = UnitListMsg(msg, Some "Attacker")
let defenderMap msg = UnitListMsg(msg, Some "Defender")

let init () : Model * Cmd<Types.Msg> =
    let (attacker,attackerCmd) = MathHammer.UnitList.State.init "Attacker" () 
    let (defender,defenderCmd) = MathHammer.UnitList.State.init "Defender" () 
    
    let model : Model = 
        
        { 
            Attacker = { attacker with BoxFill="#FFCCCC"; ElementFill="#79CE0B"; ElementStroke="#396302"; OffsetY = ft.ToMM 2<ft>; Scale="scale(1,-1)" }
            Defender = { defender with BoxFill="#CCCCFF"; ElementFill="#0B79CE"; ElementStroke="#023963" }
            SelectedAttacker = None
            SelectedDefender = None
            Environment = Map.empty<_,_>
            Board = 6<ft>,4<ft>
        }
    model, Cmd.batch [ Cmd.map attackerMap attackerCmd
                       Cmd.map defenderMap defenderCmd
                       Cmd.ofMsg RebindEnvironment ]
open Probability
open Result

let pass i = float i |> Pass
let fail i = float i  |> Fail
let dPlus plus die = dist {
      let! roll = die
      let result = 
            if roll >= plus then pass roll
            else fail roll
      return result
}
let d6 = uniformDistribution [1..6]
let d3 = uniformDistribution [1..3]
let rec reduceDie d : Distribution<_> = 
      match d with 
      | D3 -> d3
      | D6 -> d6
      | Reroll(rerolls, d) -> 
            dist {
                  let! roll = reduceDie d
                  if List.contains roll rerolls then
                        return! reduceDie d
                  else return roll                        
            }
let reduceGamePrimitive = function
      | Int i -> always i
      | Dice d -> reduceDie d 
                       
let rec reduce env operation = 
      match operation with
      | Value v -> env,reduceGamePrimitive v |> Probability.map pass
      | NoValue -> env,always (fail 0)
      | Total ([]) -> env,always (pass 0)
      | Total (op::rest) -> 
            let state = reduce env op
            rest 
            |> List.fold (fun (env1,reduced1) op -> 
                  let (env2,reduced2) = reduce env op
                  env2, dist {
                        let! a' = reduced1
                        let! b' = reduced2
                        return a' + b'                              
                  }) state
      | Multiply (op,op2) -> 
            let (env1,reduced1) = reduce env op
            let (env2,reduced2) = reduce env1 op2
            env2,
            dist {

                  let! result1 = reduced1
                  let! result2 = reduced2
                  return result1 * result2
            }
      | DPlus(d, moreThan) -> env,reduceDie d |> dPlus moreThan
      | Count ([]) -> env,always (pass 0)
      | Count (op::rest) -> 
            let toCount (env1,result) =
                  env1, dist {
                        let! result = result 
                        return match result with | Pass _ -> List [pass 1; fail 0] | Fail _ -> List [pass 0; fail 1] | _ -> failwith "Cannot count these" 
                  }
            let state = reduce env op |> toCount
            rest 
            |> List.fold (fun (env1,reduced1) op -> 
                  let (env2,reduced2) = reduce env1 op |> toCount
                  env2,dist {
                        let! count1 = reduced1
                        let! count2 = reduced2 
                        return count1 + count2
                  }                  
            ) state
      | Var (scope,var)  -> env,(Map.tryFind (scope,var) env |> function Some v -> v | None -> reduce env NoValue |> snd )
      | Let(scope, var, op) -> 
            let (newEnv,result) = reduce env op
            Map.add (scope,var) result newEnv, result
let update msg model : Model * Cmd<Types.Msg> =
    match msg with
    | UnitListMsg (UnitList.Types.ModelMsg((MathHammer.Models.Types.Msg.Select), m), Some "Attacker") -> 
        let m' = model.Attacker.Models |> Map.find m
        {model with SelectedAttacker = Some m'}, Cmd.ofMsg RebindEnvironment
    | UnitListMsg (UnitList.Types.ModelMsg((MathHammer.Models.Types.Msg.Select), m), Some "Defender") -> 
        let m' = model.Defender.Models |> Map.find m
        {model with SelectedDefender = Some m'}, Cmd.ofMsg RebindEnvironment
    | UnitListMsg (msg, Some "Attacker")-> 
        let (ula,ulCmdsa) = UnitList.State.update msg model.Attacker
        { model with Attacker = ula }, Cmd.batch [ Cmd.map attackerMap ulCmdsa]
    | UnitListMsg (msg, Some "Defender")-> 
        let (uld,ulCmdsd) = UnitList.State.update msg model.Defender
        { model with Defender = uld }, Cmd.batch [ Cmd.map defenderMap ulCmdsd]
    | UnitListMsg (msg, Some _)-> failwith "No list of that name"
    | UnitListMsg (msg, None) -> 
        let (ula,ulCmdsa) = UnitList.State.update msg model.Attacker
        let (uld,ulCmdsd) = UnitList.State.update msg model.Defender
        { model with Attacker = ula; Defender = uld }, Cmd.batch [ Cmd.map attackerMap ulCmdsa
                                                                   Cmd.map defenderMap ulCmdsd ]
    | Swap -> { model with Attacker = { model.Attacker with Models = Map.map (fun k m -> {m with Attributes = List.map(function (name,Let(Defender,str,op)) -> name,Let(Attacker,str,op) | op -> op ) m.Attributes    } ) model.Defender.Models}    
                           Defender = { model.Defender with Models = Map.map (fun k m -> {m with Attributes = List.map(function (name,Let(Attacker,str,op)) -> name,Let(Defender,str,op) | op -> op ) m.Attributes    } ) model.Attacker.Models} 
                           SelectedAttacker = None }, Cmd.ofMsg ((fun msg -> UnitListMsg(msg, None)) UnitList.Types.Distribute)
    | RebindEnvironment ->
        { model with Environment = Map.empty<_,_> }, Cmd.batch [ Cmd.ofMsg BindDefender 
                                                                 Cmd.ofMsg BindAttacker ]
    | BindDefender -> 
        match model.SelectedDefender with 
        | None -> model, Cmd.none
        | Some defender -> 
            {model with Environment = defender.Attributes |> List.fold (fun env (name,op) -> reduce env op |> fst) model.Environment}, Cmd.none

    | BindAttacker -> failwith "Not Implemented"