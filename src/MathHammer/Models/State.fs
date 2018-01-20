module MathHammer.Models.State

open Elmish
open Types
open GameActions.Primitives.Types
open GameActions.Primitives.State
open TypeChecker
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
let rec evalCall dieFunc func v env  =
    let repeat lam op2 = 
        let create n = List.init (max 0 n) (fun n -> match lam with | Lam _ -> App(lam,vInt n) | notLam -> notLam
                                                     |> evalOp (evalCall dieFunc) env) |> opList
        let nestCheck check op = 
            let rec fixGp check = function 
                | NoValue 
                | Str _ 
                | Int _ as gp -> gp |> check
                | Check(Check.Fail gp) -> fixGp (Check.Fail >> Check) gp
                | Check(Check.Pass gp) -> fixGp check gp
                | ParamArray ops -> List.map (fixOp check) ops |> ParamArray
                | Tuple(gp, gp2) -> Tuple(fixGp check gp, fixGp check gp2)
                | Dist(gps) -> Distribution.map (fixGp check) gps |> Dist
            and fixOp check = function 
                | Call _
                | Var _ as gp -> gp
                | PropertyGet(v, body) -> PropertyGet(v, fixOp check body)
                | App(f, value) -> App(fixOp check f, fixOp check value)
                | Lam(param, body) -> Lam(param, fixOp check body)
                | Let(v, value, body) -> Let(v, fixOp check value, fixOp check body)
                | IfThenElse(ifExpr, thenExpr, elseExpr) -> IfThenElse(fixOp check ifExpr, fixOp check thenExpr, Option.map (fixOp check) elseExpr)
                | Value v -> Value(fixGp check v)
            fixOp check op
        let rec repeatOps times : Operation =
            let rec repeatOp : GamePrimitive->Operation = function
                | NoValue -> noValue
                | Check(Check.Fail (m)) -> 
                    repeatOp m |> nestCheck (Check.Fail >> Check)
                | Check(Check.Pass (n)) -> 
                    repeatOp n |> nestCheck (Check.Pass >> Check)
                | Int (n) -> create n
                | Dist(times) -> 
                    times 
                    |> Distribution.bind(fun gp -> 
                        match repeatOps gp with 
                        | Value(Dist(gp)) -> gp 
                        | Value(gp) -> Distribution.always gp 
                        | op -> Distribution.always (ParamArray[op])) |> Dist |> Value
                | Str(_) -> failwith "Not Implemented"
                //| Float(_) -> failwith "Not Implemented"
                | ParamArray(times) -> times |> List.map(function Value(gp) -> repeatOps gp | _ -> noValue ) |> ParamArray |> Value
                | Tuple(n, m) -> 
                    match repeatOp n, repeatOp m with 
                    | Value(ParamArray(n)), Value(ParamArray(m)) -> n @ m |> ParamArray
                    | Value(n), Value(m) -> Tuple(n,m)
                    | op,op2 -> [op;op2] |> ParamArray 
                    |> Value
            repeatOp times

        let times = evalOp (evalCall dieFunc) env op2  
        match times with 
        | Value(gp) -> repeatOps gp
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
    let evalFuncAsGp = (fun v -> evalCall dieFunc func (Value v) env |> function Value(gp) -> gp | op -> ParamArray [op])           
    match func,v with 
    | Dice d, Value(NoValue) -> dieFunc d  
    | Total, (Value(Int _) as v) -> v
    | Total, (Value(Str _) as v) -> v
    | Total, (Value(NoValue) as v) -> v 
    | Total, Value(ParamArray []) -> 
        NoValue |> Value
    | Total, Value(ParamArray (head::tail)) -> 
        fold (+) tail head
    | Product, (Value(Int _) as v) -> v
    | Product, (Value(Str _) as v) -> v
    | Product, (Value(NoValue) as v) -> v 
    | Product, Value(ParamArray []) -> 
        GamePrimitive.Zero |> Value
    | Product, Value(ParamArray (head::tail)) -> 
        fold (*) tail head
    | Count, Value(Int _) -> vInt 1
    | Count, Value(Str _) -> vInt 1
    | Count, Value(NoValue) -> vInt 0 
    | Count, Value(ParamArray []) -> 
        let zero = GamePrimitive.Zero 
        (zero,zero) |> tuple |> Value
    | Count, Value(ParamArray ops) ->
        let rec toCount result = 
            match result with 
            | Int _               -> Int(1) 
            | Str(_)              -> Int(1) 
            | NoValue             -> Int(0)     
            | Dist d              -> Distribution.map (toCount) d |> Dist                  
            | ParamArray []       -> Int(0)
            | ParamArray ops      -> ops |> List.map (function | Value(gp) -> toCount gp |> Value | op -> op ) |> ParamArray
            | Check(Check.Pass gp)-> Check.Pass (toCount gp) |> Check
            | Tuple (n, m)        -> tuple(toCount n,toCount m)
            | Check(Check.Fail gp) -> Check.Fail (toCount gp) |> Check
        let zero = GamePrimitive.Zero 
        zero
        |> Value
        |> fold (fun r1 r2 -> r1 + toCount r2) ops
    | GreaterThan, Value(ParamArray([Value(gp);Value(gp2)])) -> greaterThan gp gp2 |> Value
    | Equals, Value(ParamArray([Value(gp);Value(gp2)])) -> equals gp gp2  |> Value
    | LessThan, Value(ParamArray([Value(gp);Value(gp2)])) -> notEquals gp gp2  |> Value
    | NotEquals, Value(ParamArray([Value(gp);Value(gp2)])) -> lessThan gp gp2  |> Value
    | And, Value(ParamArray([Value(gp);Value(gp2)])) -> andGp gp gp2 |> Value
    | Or,       Value(ParamArray([Value(gp);Value(gp2)])) -> orGp gp gp2  |> Value
    | Repeat, Value(ParamArray([lam;op2])) -> repeat lam op2 
    | (Total|Product|Count), Value(Check v) -> Check.map evalFuncAsGp v |> Check |> Value
    | (Total|Product|Count), Value(Tuple(t1,t2)) -> Tuple(evalFuncAsGp t1, evalFuncAsGp t2) |> Value
    | (Total|Product|Count), Value(Dist d) -> 
        // Distribution.map evalFuncAsGp d |> Dist |> Value

        Distribution.bind (fun v -> let value = evalCall dieFunc func (Value v) env 
                                    value |> function 
                                        | Value(Dist(gp)) -> gp 
                                        | IsList(Distr _) & Value(ParamArray ops) -> 
                                            ops 
                                            |> List.map(function 
                                                        | Value(Dist(d)) -> d 
                                                        | Value(gp) -> Distribution.always gp 
                                                        | op -> ParamArray[op] |> Distribution.always)
                                            |> Distribution.combine
                                        | Value(gp) -> Distribution.always gp 
                                        | op -> Distribution.always (ParamArray [op]))  d |> Dist |> Value 
    | _ -> failwith "Cannot eval any other call with those params" 

let standardCall = (evalCall (evalDie >> Distribution.map (Int)  >> Dist >> Value))
let sampleCall = (evalCall (evalDie >> Distribution.sample >> Int >> Value))
//let avgCall = (evalCall (evalDie >> Distribution.expectation(float) >> Float >> Value))
let (|ContainsVar|_|) env key = Map.tryFind key env
let update msg model =
      match msg with
      | ChangePosition (x,y,scale) -> {model with PosX = x; PosY = y; Scale=scale}, Cmd.none
      | Select _ -> model, Cmd.none
      | Rebind (initial) -> 
            let normalized = model.Rules |> normalize
            let sampled = normalized |> evalOp sampleCall initial
            //let average = normalized |> evalOp avgCall initial
            let probability = normalized |> evalOp standardCall initial
            let cmds = []
            { model with SampleRules = sampled
                         AverageRules = probability
                         NormalizedRules = normalized
                         ProbabilityRules = probability }, Cmd.batch cmds
