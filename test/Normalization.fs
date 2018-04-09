module NormalizationTests

open Expecto
open GameActions.Primitives.Types
open GameActions.Primitives.State
let (==?) actual expected = Expect.equal expected actual ""
let rec numify i = function 
| Lam(p,b) -> Lam(i.ToString(), subst (Var (i.ToString())) p b |> numify (i + 1))
| Call x -> Call x
| PropertyGet(x, b) -> PropertyGet(x, numify i b)
| Value(ParamArray(ops)) -> ops |> List.map (numify i) |> ParamArray |> Value
| Choice(name, choices) -> Choice(name, choices |> List.map (fun (key,op) -> key,numify i op))
| Value x -> Value x
| Var x -> Var x
| App(f, value) -> App(numify i f, numify i value) 
| Let(x, value, body) -> Let(x, numify i value, numify (i + 1) body)
| IfThenElse(ifExpr, thenExpr, Some elseExpr) -> IfThenElse( numify i ifExpr,  numify i thenExpr, Some ( numify i elseExpr))
| IfThenElse(ifExpr, thenExpr, None) -> IfThenElse( numify i ifExpr,  numify i thenExpr, None)

let zero = Lam("f",Lam("x", Var("x")))// fun f -> fun x -> x
let one = Lam("f",Lam("x", App(Var "f", Var("x"))))// fun f -> fun x -> f x
let two = Lam( "f", Lam("x",App(Var("f"),App(Var("f"),Var("x"))))) //fun f -> fun x -> f f x
let three = Lam( "f", Lam("x",App(Var("f"),App(Var("f"),App(Var("f"),Var("x")))))) //fun f -> fun x -> f f f x
let ``Lambda Calculus`` = 
    let rec doAdding op =
        match op with 
        | App(Value(Str "Add 1"), op2) -> 1 + doAdding op2
        | Value(Str "Zero") -> 0
        | x -> failtest (sprintf "Couldn't add unexpected value %A" x)
    let doTest (op,expected) = 
        let result = 
            op <*> Value(Str "Add 1") <*> Value(Str "Zero")
            |> normalize |> snd |> doAdding    
        test (sprintf "Lambda for %d" expected) { result ==? expected }
    testList "Lambda Calculus Normalization" (List.map doTest [zero,0;one,1;two,2;three,3])

let ``Ski Combinators`` =
    let i = Lam("x", Var "x")
    let k = Lam("x", Lam ("y", Var "x"))
    let xz = App(Var "x", Var"z")
    let yz = App(Var "y", Var "z")
    let ``xz(yz)`` = App(xz,yz)
    let s = Lam("x", Lam ("y", Lam ("z", ``xz(yz)``)))
    
    let ``false`` = App(s,k)
    let ``true`` = k 
    let ``not`` = App(``false``,``true``)
    let ``or`` = k
    let ``and`` = ``false``

    testList "Ski Combinators" [
        test "I is identity" {
            let x' = vInt 6
            App(i, x') |> normalize |> snd ==? x'
        }
        test "K returns first" {
            let x' = vInt 6
            let y' = vInt 8
            App(App(k, x'),y') |> normalize |> snd ==? x'
        }        
        testList "Test Boolean Functions" [
            test "F returns second" {
                let x' = vInt 6
                let y' = vInt 8
                App(App(``false``, x'),y') |> normalize |> snd ==? y'          
            }
            test "T returns first" {
                let x' = vInt 6
                let y' = vInt 8
                App(App(``true``, x'),y') |> normalize |> snd ==? x'          
            }
            test "NOT = (SK)(K)" {
                let ``(SK)(K)`` = (App(App(s,k), k)) |> normalize |> snd             
                (``not`` |> normalize |> snd |> numify 0) ==? (``(SK)(K)``  |> numify 0)
            }
            test "True Not == False " {
                (App(``true``,``not``) |> normalize |> snd |> numify 0) ==? (``false`` |> normalize |> snd |> numify 0) 
            }
            test "False Not == False " {
                (App(``false``,``not``) |> normalize |> snd |> numify 0) ==? (``true`` |> normalize |> snd |> numify 0) 
            }     
            test "x OR y" {
                (App(``true``,App(``or``,``true``))  |> normalize |> snd |> numify 0) ==? (``true``  |> normalize |> snd |> numify 0)  
                (App(``false``,App(``or``,``true``)) |> normalize |> snd |> numify 0) ==? (``true``  |> normalize |> snd |> numify 0)  
                (App(``true``,App(``or``,``false``)) |> normalize |> snd |> numify 0) ==? (``true``  |> normalize |> snd |> numify 0)  
                (App(``false``,App(``or``,``false``))|> normalize |> snd |> numify 0) ==? (``false`` |> normalize |> snd |> numify 0)                 
            } 
            test "x AND y" {
                (App(``true``,App(``true``,``and``))  |> normalize |> snd |> numify 0) ==? (``true``  |> normalize |> snd |> numify 0)  
                (App(``false``,App(``true``,``and``)) |> normalize |> snd |> numify 0) ==? (``false`` |> normalize |> snd |> numify 0)  
                (App(``true``,App(``false``,``and``)) |> normalize |> snd |> numify 0) ==? (``false`` |> normalize |> snd |> numify 0)  
                (App(``false``,App(``false``,``and``))|> normalize |> snd |> numify 0) ==? (``false`` |> normalize |> snd |> numify 0)     
            }
        ]
    ]
let ``Closure test`` = 
    test "Inside lam scopes correctly" {
        App(Lam("y",App(Lam("y", Var "y"),Value(Int(7)))),Value(Int(9))) |> normalize |> snd ==? Value(Int(7))
    }
let ``Getting a property test`` =
    let six = Value(Str "six") //Value(Float 6.)  
    let c = opList [opList [Value (Str "M"); Value(Int 8)]; opList [Value (Str "T"); six]]
    testList "Getting a property test" [
        test "Get First Property" {
            PropertyGet("M", c) |> normalize |> snd ==? Value(Int 8)    
        }
        test "Get Second Property" {
            PropertyGet("T", c) |> normalize |> snd ==? six
        }
        test "Can't find property does nothing" {
            PropertyGet("H", c) |> normalize |> snd ==? PropertyGet("H", c)    
        }
        // 
    ]
let ``Choices`` =
    let choices = 
        Choice ("Phase",
                    [("Assault", Value(NoValue))
                     ("Movement", Value(NoValue))
                     ("Shooting", Value(NoValue))])
    let choiceTest = Map.ofList [("Test", Set.ofList ["Op"; "Op2"])]                       
    let expected = Map.ofList [("Phase", Set.ofList ["Assault"; "Movement";"Shooting"])]   
                      
    let choices2 = 
        Choice ("Phase",
                    [("Psychic", Value(NoValue))
                     ("Assault", Value(NoValue))])
    let expected2 = Map.ofList [("Phase", Set.ofList ["Assault"; "Psychic"])]  
    let expectedBoth = Map.ofList [("Phase", Set.ofList ["Assault"; "Psychic";"Assault"; "Movement";"Shooting"])]   
    let v = ParamArray [choices]
    let v2 = ParamArray [choices2]
    let t = Tuple(v,v2)
    let b = ParamArray [choices; choices2]
    let op1 = Value(Check(Check.Pass v)) 
    let op2 = Value(Check(Check.Fail v2)) 
    let op3 = Value(Check(Check.Fail t)) 
    let op4 = Value(v) 
    let op5 = Value(v2) 
    let op6 = Value(b) 
    let op7 = Value(Check(Check.Fail b)) 
    let op8 = Value(ParamArray [ Value v ]) 
    let op9 = Value(ParamArray [ Value v2 ]) 
    let op10 = Value(ParamArray [ Value b ]) 
    let op11 = Value(Tuple ( v , v)) 
    let op12 = Value(Tuple ( v2 , v2 )) 
    let op13 = Value(Tuple ( v, v2 )) 
    let op14 = Value(Tuple ( t, v2 )) 
    let op15 = [v] |> Distribution.uniformDistribution |> Dist |> Value 
    let op16 = [v;v] |> Distribution.uniformDistribution |> Dist |> Value 
    let op17 = [v2;v2] |> Distribution.uniformDistribution |> Dist |> Value 
    let op18 = [t] |> Distribution.uniformDistribution |> Dist |> Value 
    let op19 = [b] |> Distribution.uniformDistribution |> Dist |> Value 
    let op20 = [v;v2] |> Distribution.uniformDistribution |> Dist |> Value 
    let op21 = [b;t] |> Distribution.uniformDistribution |> Dist |> Value 


    let ops = [ op1 
                op4 
                op8 
                op11
                op15
                op16
                op8 
                op11
                op15
                op16 ]
    let ops2 =  [ op2 
                  op5 
                  op9 
                  op12
                  op17
                  op2 
                  op5 
                  op9 
                  op12
                  op17  ]              
    let opsBoth = [ op3 
                    op6 
                    op7 
                    op10
                    op13
                    op14
                    op18
                    op19
                    op20
                    op21 ]

    let createTests f ops title expected = 
        ops
        |> List.mapi (fun i op -> test (sprintf "%s %d" title i) { f op |> normalize |> fst ==? expected }   ) 
        |> testList title

    [     createTests id ops     "Value Fst"  expected    
          createTests id ops2    "Value Snd"  expected2   
          createTests id opsBoth "Value Both"  expectedBoth
          createTests (fun op -> PropertyGet("x", op)) ops     "PropertyGet Fst"  expected    
          createTests (fun op -> PropertyGet("x", op)) ops2    "PropertyGet Snd"  expected2   
          createTests (fun op -> PropertyGet("x", op)) opsBoth "PropertyGet Both" expectedBoth
          createTests (fun (op,op2) -> App(op, op2)) (List.zip ops    ops      ) "App Fst"  expected    
          createTests (fun (op,op2) -> App(op, op2)) (List.zip ops2   ops2     ) "App Snd"  expected2   
          createTests (fun (op,op2) -> App(op, op2)) (List.zip opsBoth opsBoth ) "App Both1" expectedBoth 
          createTests (fun (op,op2) -> App(op, op2)) (List.zip ops    ops2     ) "App Fst2"  expectedBoth    
          createTests (fun (op,op2) -> App(op, op2)) (List.zip ops2   ops      ) "App Snd2"  expectedBoth   
          createTests (fun (op,op2) -> App(op, op2)) (List.zip ops2   opsBoth  ) "App Both2" expectedBoth   
          createTests (fun (op,op2) -> App(op, op2)) (List.zip opsBoth ops     ) "App Both3" expectedBoth 
          createTests (fun op -> Lam("x", op)) ops     "Lam Fst"  expected    
          createTests (fun op -> Lam("x", op)) ops2    "Lam Snd"  expected2   
          createTests (fun op -> Lam("x", op)) opsBoth "Lam Both" expectedBoth
          
          createTests (fun (op,op2) -> Let("x", op, op2)) (List.zip ops    ops      ) "Let Fst"  expected    
          createTests (fun (op,op2) -> Let("x", op, op2)) (List.zip ops2   ops2     ) "Let Snd"  expected2   
          createTests (fun (op,op2) -> Let("x", op, op2)) (List.zip opsBoth opsBoth ) "Let Both1" expectedBoth 
          createTests (fun (op,op2) -> Let("x", op, op2)) (List.zip ops    ops2     ) "Let Fst2"  expectedBoth    
          createTests (fun (op,op2) -> Let("x", op, op2)) (List.zip ops2   ops      ) "Let Snd2"  expectedBoth   
          createTests (fun (op,op2) -> Let("x", op, op2)) (List.zip ops2   opsBoth  ) "Let Both2" expectedBoth   
          createTests (fun (op,op2) -> Let("x", op, op2)) (List.zip opsBoth ops     ) "Let Both3" expectedBoth 

          createTests (fun (op,op2) -> IfThenElse(op, op2, None)) (List.zip ops    ops      ) "IfThenElse Fst"  expected    
          createTests (fun (op,op2) -> IfThenElse(op, op2, None)) (List.zip ops2   ops2     ) "IfThenElse Snd"  expected2   
          createTests (fun (op,op2) -> IfThenElse(op, op2, None)) (List.zip opsBoth opsBoth ) "IfThenElse Both1" expectedBoth 
          createTests (fun (op,op2) -> IfThenElse(op, op2, None)) (List.zip ops    ops2     ) "IfThenElse Fst2"  expectedBoth    
          createTests (fun (op,op2) -> IfThenElse(op, op2, None)) (List.zip ops2   ops      ) "IfThenElse Snd2"  expectedBoth   
          createTests (fun (op,op2) -> IfThenElse(op, op2, None)) (List.zip ops2   opsBoth  ) "IfThenElse Both2" expectedBoth   
          createTests (fun (op,op2) -> IfThenElse(op, op2, None)) (List.zip opsBoth ops     ) "IfThenElse Both3" expectedBoth 

          
          createTests (fun (op,op2) -> IfThenElse(op, op, Some op2)) (List.zip ops    ops      ) "Else Fst"  expected    
          createTests (fun (op,op2) -> IfThenElse(op, op, Some op2)) (List.zip ops2   ops2     ) "Else Snd"  expected2   
          createTests (fun (op,op2) -> IfThenElse(op, op, Some op2)) (List.zip opsBoth opsBoth ) "Else Both1" expectedBoth 
          createTests (fun (op,op2) -> IfThenElse(op, op, Some op2)) (List.zip ops    ops2     ) "Else Fst2"  expectedBoth    
          createTests (fun (op,op2) -> IfThenElse(op, op, Some op2)) (List.zip ops2   ops      ) "Else Snd2"  expectedBoth   
          createTests (fun (op,op2) -> IfThenElse(op, op, Some op2)) (List.zip ops2   opsBoth  ) "Else Both2" expectedBoth   
          createTests (fun (op,op2) -> IfThenElse(op, op, Some op2)) (List.zip opsBoth ops     ) "Else Both3" expectedBoth 
          
          
          createTests (fun (op,op2) -> Choice("Test", ["Op",op; "Op2",op2])) (List.zip ops    ops      ) "Choice Fst"   (Map.mergeSets expected     choiceTest )
          createTests (fun (op,op2) -> Choice("Test", ["Op",op; "Op2",op2])) (List.zip ops2   ops2     ) "Choice Snd"   (Map.mergeSets expected2    choiceTest )
          createTests (fun (op,op2) -> Choice("Test", ["Op",op; "Op2",op2])) (List.zip opsBoth opsBoth ) "Choice Both1" (Map.mergeSets expectedBoth choiceTest ) 
          createTests (fun (op,op2) -> Choice("Test", ["Op",op; "Op2",op2])) (List.zip ops    ops2     ) "Choice Fst2"  (Map.mergeSets expectedBoth choiceTest )    
          createTests (fun (op,op2) -> Choice("Test", ["Op",op; "Op2",op2])) (List.zip ops2   ops      ) "Choice Snd2"  (Map.mergeSets expectedBoth choiceTest )   
          createTests (fun (op,op2) -> Choice("Test", ["Op",op; "Op2",op2])) (List.zip ops2   opsBoth  ) "Choice Both2" (Map.mergeSets expectedBoth choiceTest )   
          createTests (fun (op,op2) -> Choice("Test", ["Op",op; "Op2",op2])) (List.zip opsBoth ops     ) "Choice Both3" (Map.mergeSets expectedBoth choiceTest ) ]
        |> testList "Choices"
let ``Counting call test`` = 
    let appliedTwo = two <*> vInt 7 <*> vInt 7
    testList "Normalize function application" [
        test "Count by Param Array" {
            let count ops = App(Call Count,opList (ops))

            let normalizedFirst = count [normalize appliedTwo |> snd;normalize appliedTwo |> snd;normalize appliedTwo |> snd] |> normalize |> snd
            let normalizedSecond = count [appliedTwo;appliedTwo;appliedTwo] |> normalize |> snd

            normalizedFirst ==? normalizedSecond
        }
        test "Count by repeat" {
            let count ops = App(Call Count,App(Call Repeat, opList(ops)))
            let normalizedFirst = count [normalize appliedTwo |> snd;normalize appliedTwo |> snd;normalize appliedTwo |> snd]|> normalize |> snd
            let normalizedSecond = count  [appliedTwo;appliedTwo;appliedTwo]  |> normalize |> snd

            normalizedFirst ==? normalizedSecond
        }
    ]
[<Tests>]
let tests =  
    testList "Normalization Tests" 
        [ ``Counting call test``
          ``Getting a property test``
          ``Lambda Calculus``
          ``Ski Combinators``
          ``Closure test``
          ``Choices`` ]