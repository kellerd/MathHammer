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
open Distribution
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
open Determinism      

let rec unfold callback env op op2  = 
      let (env,times) = reduce env op2  
      let newTimes = 
            times |> Distribution.map(fun times' ->
                        let newOps = 
                              match times' with  
                              | Tuple(n,_) -> List.init n (fun _ -> op) 
                              | Pass x -> List.init (int x)  (fun _ -> op) 
                              | Fail x -> List.init (int x) (fun _ -> op) 
                              | List xs -> 
                                    let n = List.fold (fun c elem -> match elem with Pass _ -> c + 1 | Fail _ -> c | Tuple(x,_) -> c + x | List _ -> failwith "Cannot count these") 0 xs
                                    List.init n  (fun _ -> op) 
                        reduce env (callback(OpList newOps))
            ) 
      match newTimes |> fromDistribution with 
      | NoResult -> reduce env NoValue             
      | Deterministic d -> d
      | NonDeterministic _ -> reduce env NoValue  
and fold folder env = function
| [] -> (env,always (pass 0))
| op::rest -> 
      let state = reduce env op
      rest 
      |> List.fold (fun (env1,reduced1) op -> 
            let (env2,reduced2) = reduce env1 op
            env2, dist {
                  let! a' = reduced1
                  let! b' = reduced2
                  return folder a' b'                              
            }) state
and reduce (env:Environment) operation = 
      match operation with
      | Value v -> (env,reduceGamePrimitive v |> Distribution.map pass) 
      | NoValue -> (env,always (fail 0)) 
      | Total (OpList ops) -> fold (+) env ops
      | Total (Unfold(op,op2)) -> unfold Total env op op2 
      | Product (Unfold(op,op2)) -> unfold Product env op op2 
      | Count (Unfold(op,op2)) -> unfold Count env op op2 
      | Product (OpList ops) -> fold (*) env ops
      | DPlus(d, moreThan) -> env,reduceDie d |> dPlus moreThan
      | Count (OpList ops) -> 
            let addCounts r1 r2 =
                  let toCount result =  
                        match result with | Pass _ -> Tuple (1,0) | Fail _ ->  Tuple(0,1) | _ -> failwith "Cannot count these" 
                  toCount r1 + toCount r2                        
            let (env,r) = fold addCounts env ops 
            printDistribution r;
            env,r  |> List.map (fun (List counts, prob) -> counts |> List.filter(function Pass c | Fail c when c <> 0. -> true | _ -> false) |> List,prob)                 
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

    | BindAttacker -> 
        match model.SelectedAttacker with 
        | None -> model, Cmd.none
        | Some attacker -> 
            {model with Environment = attacker.Attributes |> List.fold (fun env (name,op) -> reduce env op |> fst) model.Environment}, Cmd.none
