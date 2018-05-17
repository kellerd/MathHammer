module EvalTests

open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open FsCheckGen

let (==?) x y = Expect.equal x y ""

[<Tests>]
let tests =
    let evalAndRemoveLamdas eval op =
        let rec doEval =
            function 
            | Lam(_) as lam -> 
                App(lam, vInt 3)
                |> eval
                |> doEval
            | op -> op
        op
        |> eval
        |> doEval
        |> normalize
    
    let ``Normalize and eval should be the same with`` op =
        let result =
            try 
                op
                |> evalAndRemoveLamdas (evalOp Map.empty<_, _>)
                |> Choice1Of2
            with ex -> Choice2Of2(ex.Message.Substring(0, 15))
        
        let expected =
            try 
                op
                |> normalize
                |> snd
                |> evalAndRemoveLamdas (evalOp Map.empty<_, _>)
                |> Choice1Of2
            with ex -> Choice2Of2(ex.Message.Substring(0, 15))
        
        result ==? expected
    
    testList "Test normalize and eval" 
        [ testPropertyWithConfig config "Std Call normalize then eval, same as eval" (``Normalize and eval should be the same with``) ]
