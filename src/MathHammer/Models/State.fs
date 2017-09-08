module MathHammer.Models.State

open Elmish
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.State
open Distribution
open Result

let init name =
    { PosX=0.
      PosY=0.
      Name=name
      Size = 28<mm>
      Attributes = []
      Scale = "scale(1,1)"
      Rules = noValue
      SampleRules = noValue
      AverageRules = noValue
      ProbabilityRules = noValue}

let initMeq name coreRules =
    let attributes = [vInt 6; dPlus D6 3 ;dPlus D6 3; vInt 4; vInt 4; vInt 1; vInt 2; vInt 8; dPlus D6 3; Value(NoValue)] 
    let rules = applyArgs coreRules attributes
    { (init name) with Rules = rules; Attributes = attributes }, Cmd.none
let initGeq name coreRules =
    let attributes = [vInt 30; vInt 5; dPlus D6 4 ;dPlus D6 4; vInt 3; vInt 3; vInt 1; vInt 1; vInt 6; dPlus D6 5; Value(NoValue)] 
    let rules = applyArgs (coreRules |> lam "WeaponRange") attributes
    { (init name) with Rules = rules; Attributes = attributes }, Cmd.none
let rec evalDie d : Distribution<_> = 
      match d with 
      | D3 -> uniformDistribution [1..3]
      | D6 -> uniformDistribution [1..6]
      | Reroll(rerolls, d) -> 
            dist {
                  let! roll = evalDie d
                  if List.contains roll rerolls then
                        return! evalDie d
                  else return roll                        
            }
open Determinism 
let rec unfold op op2 env = 
    let times = evalOp evalCall env op2  
    let newTimes = 
        match times with 
        | Value(Dist(times)) ->
            times |> Distribution.map(fun times' ->
                        let newOps = 
                              match times' with
                              | Result(Fail (Int(n))) -> []
                              | Int (n) 
                              | Result(Tuple(Int(n),_))
                              | Result(Pass (Int(n))) -> List.init n (fun _ -> op) 
                              | Result(List xs) -> 
                                    let n = 
                                        List.fold (fun c elem -> 
                                                    match elem with 
                                                    | Pass _ -> c + 1 
                                                    | Fail _ -> c 
                                                    | Tuple(Int(x),_) -> c + x 
                                                    | _ -> failwith <| sprintf "Cannot unfold by these types of values %A" elem) 0 xs
                                    List.init n  (fun _ -> op) 
                              | elem -> failwith <| sprintf "Cannot unfold by these types of values %A" elem
                        evalOp evalCall env (ParamArray(OpList newOps))
            ) 
        | _ -> always <| Value(NoValue)
    match newTimes |> fromDistribution with 
    | NoResult -> (Value(NoValue))           
    | Deterministic d -> d
    | NonDeterministic _ -> (Value(NoValue))
and fold folder env ops state =
      ops 
      |> List.fold (fun reduced1 op -> 
            let reduced2 = evalOp evalCall env op
            match reduced1,reduced2 with 
            | Value(Dist reduced1),Value(Dist reduced2) -> 
                dist {
                      let! a' = reduced1
                      let! b' = reduced2
                      return folder a' b'                             
                } |> Dist |> Value
            | _ -> Value(NoValue)
            ) state
and evalCall func v env  =
    match func,v with 
    | Dice d, Value(NoValue) -> evalDie d  |> Distribution.map (Int)  |> Dist |> Value
    | Total, ParamArray(OpList(ops)) -> 
        always GamePrimitive.Zero
        |> Dist
        |> Value 
        |> fold (+) env ops
    | Product, ParamArray(OpList(ops)) -> 
        Int 1
        |> Pass
        |> Result
        |> always 
        |> Dist
        |> Value 
        |> fold (*) env ops
    | Count, ParamArray(OpList(ops)) ->
        let toCount result = 
            match result with | Result(Pass _) -> Result(Tuple (Int(1),Int(0))) | Result(Fail _) ->  Result(Tuple(Int(0),(Int(1)))) | Result(Tuple _) as x -> x | x -> failwith <| sprintf "Cannot count these %A" x
        let zero = GamePrimitive.Zero 
        Tuple(zero,zero)
        |> Result 
        |> always 
        |> Dist 
        |> Value
        |> fold (fun r1 r2 -> toCount r1 + toCount r2)  env ops
    | GreaterThan, ParamArray(OpList([Value(gp);Value(gp2)])) -> greaterThan gp gp2 |> Value
    | Equals, ParamArray(OpList([Value(gp);Value(gp2)])) -> equals gp gp2  |> Value
    | LessThan, ParamArray(OpList([Value(gp);Value(gp2)])) -> notEquals gp gp2  |> Value
    | NotEquals, ParamArray(OpList([Value(gp);Value(gp2)])) -> lessThan gp gp2  |> Value
    | Unfold, ParamArray(OpList [op;op2]) -> unfold op op2 env
    | Total, Value _
    | Product,  Value _ 
    | Count,  Value _ -> evalCall func (ParamArray(OpList [v])) env
    | _ -> failwith "Cannot eval any other call with those params"  //Eval with only one operation

let update msg model =
      match msg with
      | ChangePosition (x,y,scale) -> {model with PosX = x; PosY = y; Scale=scale}, Cmd.none
      | Select _ -> model, Cmd.none
      | Msg.Let _ ->  model, Cmd.none
      | Rebind (initial) -> 
            let normalized = model.Rules |> normalizeOp
            let sampled = normalized |> evalOp evalCall initial
            let average = normalized |> evalOp evalCall initial
            let probability = normalized |> evalOp evalCall initial
            //let cmds = newEnv |> Map.toList |> List.map (fun (name,result) -> Cmd.ofMsg (Msg.Let(name,result)))
            let cmds = []
            { model with SampleRules = sampled
                         AverageRules = average
                         ProbabilityRules = probability }, Cmd.batch cmds
