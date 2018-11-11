module OperationTests

open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
open GameActions.GameActionsList.State
open FsCheckGen
open GameActions.GameActionsList.View
open GameActions.Primitives.TypeChecker

let (==?) x y = Expect.equal x y ""

[<Tests>]
let tests =
    testList "Operation Tests" [ let eval =
                                     normalize
                                     >> snd
                                     >> evalOp Map.empty
                                 yield test "WithLams gives some values" { 
                                           let t1 =
                                               Lam("X", 
                                                   Lam("Y", 
                                                       Lam("Z", 
                                                           Value(ParamArray [ Var "X"
                                                                              Var "Y"
                                                                              Var "Z" ]))))
                                           
                                           let t2 =
                                               Lam("X", 
                                                   Lam("Y", 
                                                       Lam("Z", 
                                                           Value(ParamArray [ Lam("A", Lam("B", Lam("C", Value NoValue)))
                                                                              Var "Y"
                                                                              Var "Z" ]))))
                                           
                                           let app1 = App(t1, Value(Int(2)))
                                           let app2 = App(app1, Value(Int(3)))
                                           let app3 = App(app2, Value(Int(4)))
                                           let app4 = App(t2, Value(Int(2)))
                                           let app5 = App(app4, Value(Int(3)))
                                           let app6 = App(app5, Value(Int(4)))
                                           let app7 = App(Lam("x", App(Lam("y", Value NoValue), Value NoValue)), Value NoValue)
                                           let app8 = App(Lam("x", App(Value NoValue, Value NoValue)), Value NoValue)
                                           (|WithLams|_|) t1 ==? Some(([ None; None; None ], [ "X"; "Y"; "Z" ]), 
                                                                      Value(ParamArray [ Var "X"
                                                                                         Var "Y"
                                                                                         Var "Z" ]))
                                           (|WithLams|_|) app1 ==? Some(([ Some(Value(Int(2)))
                                                                           None
                                                                           None ], [ "X"; "Y"; "Z" ]), 
                                                                        Value(ParamArray [ Var "X"
                                                                                           Var "Y"
                                                                                           Var "Z" ]))
                                           (|WithLams|_|) app2 ==? Some(([ Some(Value(Int(2)))
                                                                           Some(Value(Int(3)))
                                                                           None ], [ "X"; "Y"; "Z" ]), 
                                                                        Value(ParamArray [ Var "X"
                                                                                           Var "Y"
                                                                                           Var "Z" ]))
                                           (|WithLams|_|) app3 ==? Some(([ Some(Value(Int(2)))
                                                                           Some(Value(Int(3)))
                                                                           Some(Value(Int(4))) ], [ "X"; "Y"; "Z" ]), 
                                                                        Value(ParamArray [ Var "X"
                                                                                           Var "Y"
                                                                                           Var "Z" ]))
                                           (|WithLams|_|) app4 ==? Some(([ Some(Value(Int(2)))
                                                                           None
                                                                           None ], [ "X"; "Y"; "Z" ]), 
                                                                        Value(ParamArray [ Lam("A", Lam("B", Lam("C", Value NoValue)))
                                                                                           Var "Y"
                                                                                           Var "Z" ]))
                                           (|WithLams|_|) app5 ==? Some(([ Some(Value(Int(2)))
                                                                           Some(Value(Int(3)))
                                                                           None ], [ "X"; "Y"; "Z" ]), 
                                                                        Value(ParamArray [ Lam("A", Lam("B", Lam("C", Value NoValue)))
                                                                                           Var "Y"
                                                                                           Var "Z" ]))
                                           (|WithLams|_|) app6 ==? Some(([ Some(Value(Int(2)))
                                                                           Some(Value(Int(3)))
                                                                           Some(Value(Int(4))) ], [ "X"; "Y"; "Z" ]), 
                                                                        Value(ParamArray [ Lam("A", Lam("B", Lam("C", Value NoValue)))
                                                                                           Var "Y"
                                                                                           Var "Z" ]))
                                           (|WithLams|_|) t1
                                           |> Option.map (fun ((apps, ls), o) -> applyMany ls o apps)
                                           ==? Some t1
                                           (|WithLams|_|) app1
                                           |> Option.map (fun ((apps, ls), o) -> applyMany ls o apps)
                                           ==? Some app1
                                           (|WithLams|_|) app2
                                           |> Option.map (fun ((apps, ls), o) -> applyMany ls o apps)
                                           ==? Some app2
                                           (|WithLams|_|) app3
                                           |> Option.map (fun ((apps, ls), o) -> applyMany ls o apps)
                                           ==? Some app3
                                           (|WithLams|_|) app4
                                           |> Option.map (fun ((apps, ls), o) -> applyMany ls o apps)
                                           ==? Some app4
                                           (|WithLams|_|) app5
                                           |> Option.map (fun ((apps, ls), o) -> applyMany ls o apps)
                                           ==? Some app5
                                           (|WithLams|_|) app6
                                           |> Option.map (fun ((apps, ls), o) -> applyMany ls o apps)
                                           ==? Some app6
                                           (|WithLams|_|) app7
                                           |> Option.map (fun ((apps, ls), o) -> applyMany ls o apps)
                                           ==? Some app7
                                           (|WithLams|_|) app8
                                           |> Option.map (fun ((apps, ls), o) -> applyMany ls o apps)
                                           ==? Some app8
                                       }
                                 yield testPropertyWithConfig config "WithLams only gives as many Apps as Lams, any more is an error" <| fun op -> 
                                           let doLamTest op =
                                               match op with
                                               | WithLams((apps, ls), _) -> Expect.equal (List.length ls) (List.length apps) ""
                                               | _ -> ()
                                           doLamTest (App(op, Value NoValue))
                                           doLamTest (Lam("x", op))
                                 yield testPropertyWithConfig config "WithLams is opposite of applyMany" <| fun op -> 
                                           match op with
                                           | WithLams((apps, ls), o) -> applyMany ls o apps ==? op
                                           | op' -> op' ==? op
                                 yield testPropertyWithConfig config "AsElseIfs, if none is opposite of applyManyIfs" <| fun op -> 
                                           match op with
                                           | AsElseIfs ifThens -> applyManyIfs ifThens ==? op
                                           | op' -> op' ==? op
                                 // let x1 = IfThenElse(Value(Int(6)), Value(Int(7)), None) 
                                 // let x2 = IfThenElse(Value(Int(6)), Value(Int(7)), Some(IfThenElse(Value(Int(8)), Value(Int(9)), Some(Value(Int(10)))))) 
                                 // let x3 = IfThenElse(Value(Int(6)), Value(Int(7)), Some(IfThenElse(Value(Int(8)), Value(Int(9)), None))) 
                                 // let x4 = IfThenElse(Value(Int(6)), Value(Int(7)), Some(IfThenElse(Value(Int(8)), Value(Int(9)), Some(IfThenElse(Value(Int(10)), Value(Int(11)), Some(IfThenElse(Value(Int(12)), Value(Int(13)), Some(Value(Int(14))))))))))
                                 // let x5 = IfThenElse(Value(Int(6)), Value(Int(7)), Some(IfThenElse(Value(Int(8)), Value(Int(9)), Some(IfThenElse(Value(Int(10)), Value(Int(11)), Some(IfThenElse(Value(Int(12)), Value(Int(13)), None))))))) 
                                 // x1 |> (|AsElseIfs|_|) |> Option.bind applyManyIfs = Some x1
                                 // x2 |> (|AsElseIfs|_|) |> Option.bind applyManyIfs = Some x2
                                 // x3 |> (|AsElseIfs|_|) |> Option.bind applyManyIfs = Some x3
                                 // x4 |> (|AsElseIfs|_|) |> Option.bind applyManyIfs = Some x4
                                 // x5 |> (|AsElseIfs|_|) |> Option.bind applyManyIfs = Some x5
                                 // NoValue |> Value  |> (|AsElseIfs|_|) |> Option.bind applyManyIfs = None
                                 yield test "Evalled D6 equal std distribution of integers, reversed" { 
                                           let result = eval d6
                                           
                                           let expected =
                                               [ 1..6 ]
                                               |> List.map (Int)
                                               |> List.rev
                                               |> Distribution.uniformDistribution
                                               |> Dist
                                               |> Value
                                           result ==? expected
                                       }
                                 yield test "Evalled D3 equal std distribution of integers, reversed" { 
                                           let result = eval d3
                                           
                                           let expected =
                                               [ 1..3 ]
                                               |> List.map (Int)
                                               |> List.rev
                                               |> Distribution.uniformDistribution
                                               |> Dist
                                               |> Value
                                           result ==? expected
                                       }
                                 yield testPropertyWithConfig config "IfThenElse NoValue returns Else" <| fun thenPart elsePart -> 
                                           let ifThenElse = IfThenElse(Value NoValue, thenPart, elsePart)
                                           let result = eval ifThenElse
                                           match elsePart with
                                           | Some elsePart -> result ==? eval elsePart
                                           | None -> result ==? Value NoValue
                                 //Let x = 3 returns 3 as well as binding to environment
                                 let retValueIsSame f v =
                                     let evaled =
                                         Let("x", Value(v), Var("x"))
                                         |> f
                                         |> evalOp Map.empty
                                     
                                     let evaled' =
                                         Value(v)
                                         |> f
                                         |> evalOp Map.empty
                                     
                                     evaled ==? evaled'
                                 yield testPropertyWithConfig config "let x = 3 returns 3 evaluated without normalization" (retValueIsSame id)
                                 yield testPropertyWithConfig config "let x = 3 returns 3 evaluated with normalization" 
                                           (retValueIsSame (normalize >> snd))
                                 //Let x = some number in
                                 //x + some other number
                                 let addition x y =
                                     let result =
                                         try 
                                             Let("y", Value(y), 
                                                 App(Call Total, 
                                                     opList [ Value(x)
                                                              Var("y") ]))
                                             |> evalOp Map.empty
                                             |> Choice1Of2
                                         with ex -> Choice2Of2(ex.Message.Substring(0, 30))
                                     
                                     let expected =
                                         try 
                                             match x, y with
                                             | Dist reduced1, Dist reduced2 -> 
                                                 Distribution.dist { let! a' = reduced1
                                                                     let! b' = reduced2
                                                                     return a' + b' }
                                                 |> Dist
                                                 |> Value
                                                 |> Choice1Of2
                                             | Dist reduced1, b -> 
                                                 Distribution.dist { let! a' = reduced1
                                                                     return a' + b }
                                                 |> Dist
                                                 |> Value
                                                 |> Choice1Of2
                                             | a, Dist reduced2 -> 
                                                 Distribution.dist { let! b' = reduced2
                                                                     return a + b' }
                                                 |> Dist
                                                 |> Value
                                                 |> Choice1Of2
                                             | a, b -> 
                                                 a + b
                                                 |> Value
                                                 |> Choice1Of2
                                         with ex -> Choice2Of2(ex.Message.Substring(0, 30))
                                     
                                     result ==? expected
                                 yield testPropertyWithConfig config "Addition in child scope is valid" addition
                                 let totalOfXIsX x =
                                     let expected = x |> Value
                                     let result = Let("x", expected, App(Call Total, Var("x"))) |> evalOp Map.empty
                                     result ==? expected
                                 yield testPropertyWithConfig config "Total of x is x" totalOfXIsX
                                 yield test "Check primitive partial application" { 
                                           let value =
                                               Let
                                                   ("f", 
                                                    Lam("x", 
                                                        Lam("y", 
                                                            Lam("z", 
                                                                Value(ParamArray [ get "x"
                                                                                   get "y"
                                                                                   get "z" ])))), 
                                                    Let
                                                        ("g", App(get "f", Value(Int 6)), 
                                                         Let("h", App(get "g", Value(Int 7)), App(get "h", Value(Int 8)))))
                                           
                                           let result = value |> evalOp Map.empty
                                           let f x y : int -> int list = fun z -> [ x; y; z ]
                                           let g = f 6
                                           let h = g 7
                                           
                                           let expected =
                                               h 8
                                               |> List.map (Int >> Value)
                                               |> opList
                                           result ==? expected
                                       }
                                 yield test "Check primitive partial application 2" { 
                                           let value =
                                               App(App(Lam("X", 
                                                           Lam("Y", 
                                                               Value(ParamArray [ get "X"
                                                                                  get "Y" ]))), Value(Int 2)), Value(Int 3))
                                           
                                           let result = value |> evalOp Map.empty
                                           
                                           let expected =
                                               (fun x y -> [ x; y ]) 2 3
                                               |> List.map (Int >> Value)
                                               |> opList
                                           result ==? expected
                                       }
                                 let partialApplication =
                                     let xs =
                                         FsCheckGen.genListOfPrimitive
                                         |> FsCheck.Gen.sample 1
                                         |> Array.head
                                         |> List.map Value
                                     
                                     let vars = xs |> List.mapi (fun i _ -> (string i))
                                     
                                     let lams =
                                         vars
                                         |> List.rev
                                         |> List.fold (fun op var -> Lam(var, op)) (vars
                                                                                    |> List.map (Var)
                                                                                    |> ParamArray
                                                                                    |> Value)
                                     
                                     let apps = xs |> List.fold (fun op v -> App(op, v)) lams
                                     let result = apps |> (evalOp Map.empty)
                                     let expected = Value(ParamArray(List.map (evalOp Map.empty) xs))
                                     result ==? expected
                                 yield testPropertyWithConfig config "Partial application is in the correct order" partialApplication ]
