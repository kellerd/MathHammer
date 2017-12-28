module MathHammer.Models.State

open Elmish
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.State

let init name =
    { PosX=0.
      PosY=0.
      Name=name
      Size = 28<mm>
      Attributes = []
      Scale = "scale(1,1)"
      Rules = noValue
      SampleRules = noValue
      NormalizedRules = noValue
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
let rec evalDie d : Distribution.Distribution<_> = 
      match d with 
      | D3 -> Distribution.uniformDistribution [3..-1..1]
      | D6 -> Distribution.uniformDistribution [6..-1..1]
      | Reroll(rerolls, d) -> 
            Distribution.dist {
                  let! roll = evalDie d
                  if List.contains roll rerolls then
                        return! evalDie d
                  else return roll                        
            }

                // | Check(Check.List xs) -> 
                //     let n = 
                //         List.fold (fun c elem -> 
                //                     match elem with 
                //                     | Check.Pass _ -> c + 1 
                //                     | Check.Fail _ -> c 
                //                     | Check.Tuple(Int(x),_) -> c + x 
                //                     | _ -> c) 0 xs
                //     List.init n  (fun n -> App(lam,vInt n)) 
let rec evalCall dieFunc func v env  =
    //let lam = Lam ("unusedVariable",Value (Dist [(Float 1.0, 0.3636363636)]))
    //let op2 = Value (Dist [(Float 1.0, 0.3636363636)])
    //let dieFunc = (evalDie >> Distribution.map (Int)  >> Dist >> Value)
    //let env = Map.empty<_,_>
    //let times = [(Float 1.0, 0.3636363636)]
    let repeat lam op2 = 
        let rec repeatOps lam times : Operation =
            let rec repeatOp : GamePrimitive->Operation = function
                | NoValue
                | Check(Check.Fail (_)) -> noValue
                | Int (n) -> List.init (max 0 n) (fun n -> App(lam,vInt n)) |> opList
                | Dist(times) -> times |> Distribution.map(fun gp -> [repeatOps lam gp] |> ParamArray) |> Dist |> Value
                | Str(_) -> failwith "Not Implemented"
                //| Float(_) -> failwith "Not Implemented"
                | Check(Check.Pass (n)) -> repeatOp n
                | ParamArray(times) -> times |> List.map(function Value(gp) -> repeatOps lam gp | _ -> noValue ) |> ParamArray |> Value
                | Tuple(n, m) -> [Value n;Value m] |> ParamArray |> repeatOp
            evalOp (evalCall dieFunc) env (repeatOp times)

        let times = evalOp (evalCall dieFunc) env op2  
        match times with 
        | Value(gp) -> repeatOps lam gp
        | _ -> printfn "Times is not a value %A" times; noValue
    
    let fold folder ops state =
      ops 
      |> List.fold (fun reduced1 op -> 
            let reduced2 = evalOp (evalCall dieFunc) env op
            match reduced1,reduced2 with 
            | Value(Dist reduced1),Value(Dist reduced2) -> 
                Distribution.dist {
                      let! a' = reduced1
                      let! b' = reduced2
                      return folder a' b'                             
                } |> Dist |> Value
            | Value(Dist reduced1),Value(b) ->
                Distribution.dist {
                      let! a' = reduced1
                      return folder a' b                             
                } |> Dist |> Value
            |   Value(a), Value(Dist reduced2) ->
                Distribution.dist {
                      let! b' = reduced2
                      return folder a b'                             
                } |> Dist |> Value
            | Value(a), Value(b) ->
                folder a b |> Value
            | _ -> NoValue |> Value
            ) state
    match func,v with 
    | Dice d, Value(NoValue) -> dieFunc d  
    | Total, Value(ParamArray []) -> 
        GamePrimitive.Zero |> Value
    | Total, Value(ParamArray (head::tail)) -> 
        fold (+) tail head
    | Product, Value(ParamArray []) -> 
        GamePrimitive.Zero |> Value
    | Product, Value(ParamArray (head::tail)) -> 
        fold (*) tail head
    | Count, Value(ParamArray []) -> 
        let zero = GamePrimitive.Zero 
        (zero,zero) |> tuple |> Value
    | Count, Value(ParamArray ops) ->
        let toCount result = 
            match result with 
            | ParamArray _        -> tuple(Int(1),Int(0))
            | Check(Check.Pass _) -> tuple(Int(1),Int(0))
            | Tuple (_)           -> tuple(Int(1),Int(0)) 
            | Int(_)              -> tuple(Int(1),Int(0))
            | Str(_)              -> tuple(Int(1),Int(0))
            //| Float(_)            -> tuple(Int(1),Int(0))
            | Dist(_)             -> tuple(Int(1),Int(0)) 
            | NoValue             -> tuple(Int(0),Int(1))
            | Check(Check.Fail _) -> tuple(Int(0),Int(1))
        let zero = GamePrimitive.Zero 
        (zero,zero) 
        |> tuple
        |> Distribution.always 
        |> Dist 
        |> Value
        |> fold (fun r1 r2 -> r1 + toCount r2) ops
    | GreaterThan, Value(ParamArray([Value(gp);Value(gp2)])) -> greaterThan gp gp2 |> Value
    | Equals, Value(ParamArray([Value(gp);Value(gp2)])) -> equals gp gp2  |> Value
    | LessThan, Value(ParamArray([Value(gp);Value(gp2)])) -> notEquals gp gp2  |> Value
    | NotEquals, Value(ParamArray([Value(gp);Value(gp2)])) -> lessThan gp gp2  |> Value
    | And, Value(ParamArray([Value(gp);Value(gp2)])) -> andGp gp gp2 |> Value
    | Or,       Value(ParamArray([Value(gp);Value(gp2)])) -> orGp gp gp2  |> Value
    | Repeat, Value(ParamArray([lam;op2])) -> repeat lam op2 
    | Total, Value _
    | Product,  Value _ 
    | Count,  Value _ -> evalCall dieFunc func (opList [v]) env  //Eval with only one operation
    | _ -> failwith "Cannot eval any other call with those params" 

let standardCall = (evalCall (evalDie >> Distribution.map (Int)  >> Dist >> Value))
let sampleCall = (evalCall (evalDie >> Distribution.sample >> Int >> Value))
//let avgCall = (evalCall (evalDie >> Distribution.expectation(float) >> Float >> Value))
let (|ContainsVar|_|) env key = Map.tryFind key env
let update msg model =
      match msg with
      | ChangePosition (x,y,scale) -> {model with PosX = x; PosY = y; Scale=scale}, Cmd.none
      | Select _ -> model, Cmd.none
      | Msg.Let _ ->  model, Cmd.none
      | Rebind (initial) -> 
            let normalized = model.Rules |> normalizeOp 
            let sampled = normalized |> evalOp sampleCall initial
            //let average = normalized |> evalOp avgCall initial
            let probability = normalized |> evalOp standardCall initial
            let cmds = []
            { model with SampleRules = sampled
                         AverageRules = probability
                         NormalizedRules = normalized
                         ProbabilityRules = probability }, Cmd.batch cmds
