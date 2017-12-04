module EvalTests
open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open MathHammer.Models.State
open FsCheckGen

let (==?) x y = Expect.equal x y ""

let firstCall = (evalCall (evalDie >> (Seq.map (fst) >> Seq.head) >> Int >> Value))

[<Tests>]
let tests = 

    let evalAndRemoveLamdas eval op =
        let rec doEval = function 
            | Lam(_) as lam -> App(lam,vInt 3) |> eval |> doEval
            | op -> op
        op |> eval |> doEval |> normalizeOp
    let ``Normalize and eval should be the same with`` call op = 
        let result = 
            try op                |> evalAndRemoveLamdas (evalOp call Map.empty<_,_>)  |> Choice1Of2
            with ex -> Choice2Of2 (ex.Message.Substring(0,15))
        let expected = 
            try op |> normalizeOp |> evalAndRemoveLamdas (evalOp call Map.empty<_,_>)  |> Choice1Of2
            with ex -> Choice2Of2 (ex.Message.Substring(0,15))
        result ==? expected 
    testList "Test normalize and eval" [
        testPropertyWithConfig config "Std Call normalize then eval, same as eval" (``Normalize and eval should be the same with`` standardCall)
        testPropertyWithConfig config "Avg Call normalize then eval, same as eval" (``Normalize and eval should be the same with`` avgCall)
        testPropertyWithConfig config "Single Call normalize then eval, same as eval" (``Normalize and eval should be the same with`` firstCall)
    ]
