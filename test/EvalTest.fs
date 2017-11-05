module EvalTest
open Expecto
open GameActions.Primitives.State
open MathHammer.Models.State
open FsCheckGen

let (==?) x y = Expect.equal x y ""

[<Tests>]
let tests = 
    let ``Normalize and eval should be the same with`` call op = 
        let result = 
            try evalOp call Map.empty<_,_> op |> Choice1Of2
            with ex -> Choice2Of2 (ex.Message.Substring(0,30))
        let expected = 
            try normalizeOp op |> evalOp call Map.empty<_,_>   |> Choice1Of2
            with ex -> Choice2Of2 (ex.Message.Substring(0,30))
        result ==? expected 
    testList "Test normalize and eval" [
        testPropertyWithConfig config "Std Call normalize then eval, same as eval" (``Normalize and eval should be the same with`` standardCall)
        testPropertyWithConfig config "Avg Call normalize then eval, same as eval" (``Normalize and eval should be the same with`` avgCall)
        testPropertyWithConfig config "Sample Call normalize then eval, same as eval" (``Normalize and eval should be the same with`` sampleCall)
    ]