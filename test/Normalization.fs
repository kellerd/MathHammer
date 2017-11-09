module Normalization

open GameActions.Primitives.Types
open GameActions.Primitives.State
open Expecto

let (==?) actual expected = Expect.equal expected actual ""


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
        let result = Value(Str "Zero") |%> (Value(Str "Add 1") |%> op) |> normalizeOp |> doAdding    
        test (sprintf "Lambda for %d" expected) { result ==? expected }
    testList "Lambda Calculus Normalization" (List.map doTest [zero,0;one,1;two,2;three,3])

let ``Ski Combinators`` =
    let i = Lam("x", Var "x")
    let k = Lam("x", Lam ("y", Var "x"))
    let xz = App(Var "x", Var"z")
    let yz = App(Var "y", Var "z")
    let ``xz(yz)`` = App(xz,yz)
    let s = Lam("x", Lam ("y", Lam ("z", ``xz(yz)``)))
    
    let ``false`` = App(s,k) |> normalizeOp
    let ``true`` = k 
    let ``not`` = App(``false``,``true``)
    let ``or`` = k
    let ``and`` = ``false``

    testList "Ski Combinators" [
        test "I is identity" {
            let x' = vInt 6
            App(i, x') |> normalizeOp ==? x'
        }
        test "K returns first" {
            let x' = vInt 6
            let y' = vInt 8
            App(App(k, x'),y') |> normalizeOp ==? x'
        }        
        testList "Test Boolean Functions" [
            test "F returns second" {
                let x' = vInt 6
                let y' = vInt 8
                App(App(``false``, x'),y') |> normalizeOp ==? y'          
            }
            test "T returns first" {
                let x' = vInt 6
                let y' = vInt 8
                App(App(``true``, x'),y') |> normalizeOp ==? x'          
            }
            test "NOT = (SK)(K)" {
                let ``(SK)(K)`` = (App(App(s,k), k)) |> normalizeOp                
                (``not`` |> normalizeOp) ==? ``(SK)(K)`` 
            }
            test "True Not == False " {
                (App(``true``,``not``) |> normalizeOp) ==? (``false`` |> normalizeOp) 
            }
            ptest "True Not == False " {
                (App(``false``,``not``) |> normalizeOp) ==? (``true`` |> normalizeOp) 
            }     
            ptest "x OR y" {
                (App(``true``,App(``or``,``true``))|> normalizeOp) ==? (``true`` |> normalizeOp)  
                (App(``false``,App(``or``,``true``))|> normalizeOp) ==? (``true`` |> normalizeOp)  
                (App(``true``,App(``or``,``false``))|> normalizeOp) ==? (``true`` |> normalizeOp)  
                (App(``false``,App(``or``,``false``))|> normalizeOp) ==? (``false`` |> normalizeOp)                 
            } 
            ptest "x AND y" {
                (App(``true``,App(``true``,``and``))|> normalizeOp) ==? (``true`` |> normalizeOp)  
                (App(``false``,App(``true``,``and``))|> normalizeOp) ==? (``false`` |> normalizeOp)  
                (App(``true``,App(``false``,``and``))|> normalizeOp) ==? (``false`` |> normalizeOp)  
                (App(``false``,App(``false``,``and``))|> normalizeOp) ==? (``false`` |> normalizeOp)                    
            }
        ]
    ]
let ``Getting a property test`` =
    let c = ParamArray[ParamArray[Value (Str "M"); Value(Int 8)]; ParamArray[Value (Str "T"); Value(Float 6.)]]
    testList "Getting a property test" [
        test "Get First Property" {
            PropertyGet("M", c) |> normalizeOp ==? Value(Int 8)    
        }
        test "Get Second Property" {
            PropertyGet("T", c) |> normalizeOp ==? Value(Float 6.)   
        }
        test "Can't find property does nothing" {
            PropertyGet("H", c) |> normalizeOp ==? PropertyGet("H", c)    
        }
        // 
    ]
let ``Counting call test`` = 
    let appliedTwo = vInt 7 |%> (vInt 7 |%> two)
    testList "Normalize function application" [
        test "Count by Param Array" {
            let count ops = App(Call Count,ParamArray(ops))

            let normalizedFirst = count [normalizeOp appliedTwo;normalizeOp appliedTwo;normalizeOp appliedTwo] |> normalizeOp
            let normalizedSecond = count [appliedTwo;appliedTwo;appliedTwo] |> normalizeOp

            normalizedFirst ==? normalizedSecond
        }
        test "Count by repeat" {
            let count ops = App(Call Count,App(Call Repeat, ParamArray(ops)))
            let normalizedFirst = count [normalizeOp appliedTwo;normalizeOp appliedTwo;normalizeOp appliedTwo]|> normalizeOp
            let normalizedSecond = count  [appliedTwo;appliedTwo;appliedTwo]  |> normalizeOp

            normalizedFirst ==? normalizedSecond
        }
    ]
[<Tests>]
let tests =  
    testList "Normalization Tests" 
        [ ``Counting call test``
          ``Getting a property test``
          ``Lambda Calculus``
          ``Ski Combinators`` ]