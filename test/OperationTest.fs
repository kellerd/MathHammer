module OperationTests
open FsCheck
open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open FsCheckGen
open GameActions.GameActionsList.State
let (==?) x y = Expect.equal x y ""
[<Tests>]
let tests = 
    testList "Operation Tests" [
        let eval = normalize >> snd >> evalOp Map.empty<_,_> 
        yield test "Evalled D6 equal std distribution of integers, reversed" {
            let result = eval d6
            let expected = [1..6] |> List.map (Int) |> List.rev |> Distribution.uniformDistribution |> Dist |> Value
            result ==? expected
        }
        
        yield test "Evalled D3 equal std distribution of integers, reversed" {
            let result = eval d3
            let expected = [1..3] |> List.map (Int) |> List.rev |> Distribution.uniformDistribution |> Dist |> Value
            result ==? expected
        }
        yield testPropertyWithConfig config "IfThenElse NoValue returns Else" <| fun thenPart elsePart ->
            let ifThenElse = IfThenElse(Value NoValue,thenPart,elsePart)
            let result = eval ifThenElse 
            match elsePart with 
            | Some elsePart -> result ==? eval elsePart 
            | None -> result ==? Value NoValue
        
        //Let x = 3 returns 3 as well as binding to environment
        let retValueIsSame f v = 
            let evaled = Let("x", Value(v) ,Var ("x")) |> f |> evalOp Map.empty<_,_> 
            let evaled' = Value(v) |> f |> evalOp Map.empty<_,_> 
            evaled ==? evaled'
        yield testPropertyWithConfig config "let x = 3 returns 3 evaluated without normalization" (retValueIsSame id)
        yield testPropertyWithConfig config "let x = 3 returns 3 evaluated with normalization" (retValueIsSame (normalize >> snd))
        //Let x = some number in
        //x + some other number
        let addition x y  =   
            let result = 
                try 
                    Let("y", Value(y) ,App(Call Total, opList[Value(x);Var ("y")]))
                    |> evalOp Map.empty<_,_> 
                    |> Choice1Of2
                with ex -> Choice2Of2 (ex.Message.Substring(0,30))
            let expected = 
                try 
                    match x,y with 
                    | Dist reduced1,Dist reduced2 -> 
                        Distribution.dist {
                              let! a' = reduced1
                              let! b' = reduced2
                              return a' + b'                           
                        } |> Dist |> Value  |> Choice1Of2
                    | Dist reduced1,b ->
                        Distribution.dist {
                              let! a' = reduced1
                              return a' + b                         
                        } |> Dist |> Value  |> Choice1Of2
                    |   a, Dist reduced2 ->
                        Distribution.dist {
                              let! b' = reduced2
                              return a + b'                            
                        } |> Dist |> Value  |> Choice1Of2
                    | a,b ->
                        a + b |> Value  |> Choice1Of2
                with ex -> Choice2Of2 (ex.Message.Substring(0,30))
            result ==? expected 
        yield testPropertyWithConfig config "Addition in child scope is valid" addition
        let totalOfXIsX x = 
            let expected = x |> Value
            let result =
                Let("x", expected ,App(Call Total, Var("x"))) 
                |> evalOp Map.empty<_,_>
            result ==? expected
        yield testPropertyWithConfig config "Total of x is x" totalOfXIsX
        yield test "Check primitive partial application" {
            let value = 
                Let ("f",
                     Lam ("x",
                             Lam ("y",
                                     Lam ("z", Value(ParamArray[get "x"; get "y"; get "z"])))),
                     Let ("g", App (get "f", Value (Int 6)),
                          Let ("h", App (get "g", Value (Int 7)), App (get "h", Value (Int 8)))))
            let result = value |> evalOp Map.empty<_,_>   
            let f x y : int -> int list = fun z -> [x;y;z]
            let g = f 6
            let h = g 7
            let expected = h 8 |> List.map (Int >> Value) |> opList                     
            result ==? expected
        }
        yield test "Check primitive partial application 2" {
            let value = App(App(Lam("X", Lam("Y", Value(ParamArray[get "X"; get "Y"]))),Value(Int 2)),Value(Int 3))
            let result = value |> evalOp Map.empty<_,_>                          
            let expected = (fun x y -> [x;y]) 2 3 |> List.map (Int >> Value) |> opList
            result ==? expected
        }
        let partialApplication = 
            let xs = FsCheckGen.genListOfPrimitive |> FsCheck.Gen.sample 1 |> Array.head |> List.map Value
            let vars = xs |> List.mapi (fun i _ -> (string i)) 
            let lams = 
                vars
                |> List.rev
                |> List.fold (fun op var -> Lam(var, op)) (vars |> List.map (Var) |> ParamArray |> Value)
            let apps =   
                xs
                |> List.fold (fun op v -> App(op,v)) lams
            let result = apps |> (evalOp  Map.empty<_,_>)
            let expected = Value(ParamArray (List.map (evalOp Map.empty<_,_>) xs))
            result ==? expected
        yield testPropertyWithConfig config "Partial application is in the correct order" partialApplication                
    ]