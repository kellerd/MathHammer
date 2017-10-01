module MathHammer.Models.State

open Elmish
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.State
open Distribution
open Result
open Determinism 

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
    let attributes = [vInt 6; dPlus D6 3 ;dPlus D6 3; vInt 4; vInt 4; vInt 1; vInt 2; vInt 8; dPlus D6 3; noValue] 
    let rules = applyArgs coreRules attributes
    { (init name) with Rules = rules; Attributes = attributes }, Cmd.none
let initGeq name coreRules =
    let attributes = [vInt 30; vInt 5; dPlus D6 4 ;dPlus D6 4; vInt 3; vInt 3; vInt 1; vInt 1; vInt 6; dPlus D6 5; noValue] 
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

let rec evalCall dieFunc func v env  =
    let unfold op op2 = 
        let rec repeatOps op times =
            let newOps = 
                match times with
                | Result(Fail (Int(n))) -> []
                | Int (n) 
                | Result(Tuple(Int(n),_))
                | Result(Pass (Int(n))) -> List.init n (fun _ -> op) 
                | Dist(times) -> 
                    match times |> Distribution.map(repeatOps op) |> fromDistribution with
                    | NoResult -> [noValue]     
                    | Deterministic d -> [d]
                    | NonDeterministic _ -> [noValue]
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
            evalOp (evalCall dieFunc) env (ParamArray(OpList newOps))

        let times = evalOp (evalCall dieFunc) env op2  
        match times with 
        | Value(gp) -> repeatOps op gp
        | _ -> noValue
        

    let fold folder ops state =
      ops 
      |> List.fold (fun reduced1 op -> 
            let reduced2 = evalOp (evalCall dieFunc) env op
            match reduced1,reduced2 with 
            | Value(Dist reduced1),Value(Dist reduced2) -> 
                dist {
                      let! a' = reduced1
                      let! b' = reduced2
                      return folder a' b'                             
                } |> Dist |> Value
            | Value(Dist reduced1),Value(b) ->
                dist {
                      let! a' = reduced1
                      return folder a' b                             
                } |> Dist |> Value
            |   Value(a), Value(Dist reduced2) ->
                dist {
                      let! b' = reduced2
                      return folder a b'                             
                } |> Dist |> Value
            | x -> failwith <| sprintf "Not a value 2 %A" x
            ) state
    match func,v with 
    | Dice d, Value(NoValue) -> dieFunc d  
    | Total, ParamArray(OpList(ops)) -> 
        always GamePrimitive.Zero
        |> Dist
        |> Value 
        |> fold (+) ops
    | Product, ParamArray(OpList(ops)) -> 
        Int 1
        |> Pass
        |> Result
        |> always 
        |> Dist
        |> Value 
        |> fold (*) ops
    | Count, ParamArray(OpList(ops)) ->
        let toCount result = 
            match result with | Result(Pass _) -> Result(Tuple (Int(1),Int(0))) | Result(Fail _) ->  Result(Tuple(Int(0),(Int(1)))) | Result(Tuple _) as x -> x | x -> failwith <| sprintf "Cannot count these %A" x
        let zero = GamePrimitive.Zero 
        Tuple(zero,zero)
        |> Result 
        |> always 
        |> Dist 
        |> Value
        |> fold (fun r1 r2 -> toCount r1 + toCount r2) ops
    | GreaterThan, ParamArray(OpList([Value(gp);Value(gp2)])) -> greaterThan gp gp2 |> Value
    | Equals, ParamArray(OpList([Value(gp);Value(gp2)])) -> equals gp gp2  |> Value
    | LessThan, ParamArray(OpList([Value(gp);Value(gp2)])) -> notEquals gp gp2  |> Value
    | NotEquals, ParamArray(OpList([Value(gp);Value(gp2)])) -> lessThan gp gp2  |> Value
    | Unfold, ParamArray(OpList [op;op2]) -> unfold op op2 
    | Total, Value _
    | Product,  Value _ 
    | Count,  Value _ -> evalCall dieFunc func (ParamArray(OpList [v])) env  //Eval with only one operation
    | _ -> failwith "Cannot eval any other call with those params" 

let standardCall = (evalCall (evalDie >> Distribution.map (Int)  >> Dist >> Value))
let sampleCall = (evalCall (evalDie >> Distribution.sample >> Int >> Value))
let avgCall = (evalCall (evalDie >> Distribution.expectation(float) >> Float >> Value))
let update msg model =
      match msg with
      | ChangePosition (x,y,scale) -> {model with PosX = x; PosY = y; Scale=scale}, Cmd.none
      | Select _ -> model, Cmd.none
      | Msg.Let _ ->  model, Cmd.none
      | Rebind (initial) -> 
            let normalized = model.Rules |> normalizeOp
            let sampled = normalized |> evalOp sampleCall initial
            let average = normalized |> evalOp avgCall initial
            let probability = normalized |> evalOp standardCall initial
            let cmds = []
            { model with SampleRules = sampled
                         AverageRules = average
                         ProbabilityRules = probability }, Cmd.batch cmds
