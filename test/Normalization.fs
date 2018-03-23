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
            |> normalize |> doAdding    
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
            App(i, x') |> normalize ==? x'
        }
        test "K returns first" {
            let x' = vInt 6
            let y' = vInt 8
            App(App(k, x'),y') |> normalize ==? x'
        }        
        testList "Test Boolean Functions" [
            test "F returns second" {
                let x' = vInt 6
                let y' = vInt 8
                App(App(``false``, x'),y') |> normalize ==? y'          
            }
            test "T returns first" {
                let x' = vInt 6
                let y' = vInt 8
                App(App(``true``, x'),y') |> normalize ==? x'          
            }
            test "NOT = (SK)(K)" {
                let ``(SK)(K)`` = (App(App(s,k), k)) |> normalize                
                (``not`` |> normalize |> numify 0) ==? (``(SK)(K)``  |> numify 0)
            }
            test "True Not == False " {
                (App(``true``,``not``) |> normalize |> numify 0) ==? (``false`` |> normalize |> numify 0) 
            }
            test "False Not == False " {
                (App(``false``,``not``) |> normalize |> numify 0) ==? (``true`` |> normalize |> numify 0) 
            }     
            test "x OR y" {
                (App(``true``,App(``or``,``true``))  |> normalize |> numify 0) ==? (``true``  |> normalize |> numify 0)  
                (App(``false``,App(``or``,``true``)) |> normalize |> numify 0) ==? (``true``  |> normalize |> numify 0)  
                (App(``true``,App(``or``,``false``)) |> normalize |> numify 0) ==? (``true``  |> normalize |> numify 0)  
                (App(``false``,App(``or``,``false``))|> normalize |> numify 0) ==? (``false`` |> normalize |> numify 0)                 
            } 
            test "x AND y" {
                (App(``true``,App(``true``,``and``))  |> normalize |> numify 0) ==? (``true``  |> normalize |> numify 0)  
                (App(``false``,App(``true``,``and``)) |> normalize |> numify 0) ==? (``false`` |> normalize |> numify 0)  
                (App(``true``,App(``false``,``and``)) |> normalize |> numify 0) ==? (``false`` |> normalize |> numify 0)  
                (App(``false``,App(``false``,``and``))|> normalize |> numify 0) ==? (``false`` |> normalize |> numify 0)     
            }
        ]
    ]
let ``Closure test`` = 
    test "Inside lam scopes correctly" {
        App(Lam("y",App(Lam("y", Var "y"),Value(Int(7)))),Value(Int(9))) |> normalize ==? Value(Int(7))
    }
let ``Getting a property test`` =
    let six = Value(Str "six") //Value(Float 6.)  
    let c = opList [opList [Value (Str "M"); Value(Int 8)]; opList [Value (Str "T"); six]]
    testList "Getting a property test" [
        test "Get First Property" {
            PropertyGet("M", c) |> normalize ==? Value(Int 8)    
        }
        test "Get Second Property" {
            PropertyGet("T", c) |> normalize ==? six
        }
        test "Can't find property does nothing" {
            PropertyGet("H", c) |> normalize ==? PropertyGet("H", c)    
        }
        // 
    ]
let ``Counting call test`` = 
    let appliedTwo = two <*> vInt 7 <*> vInt 7
    testList "Normalize function application" [
        test "Count by Param Array" {
            let count ops = App(Call Count,opList (ops))

            let normalizedFirst = count [normalize appliedTwo;normalize appliedTwo;normalize appliedTwo] |> normalize
            let normalizedSecond = count [appliedTwo;appliedTwo;appliedTwo] |> normalize

            normalizedFirst ==? normalizedSecond
        }
        test "Count by repeat" {
            let count ops = App(Call Count,App(Call Repeat, opList(ops)))
            let normalizedFirst = count [normalize appliedTwo;normalize appliedTwo;normalize appliedTwo]|> normalize
            let normalizedSecond = count  [appliedTwo;appliedTwo;appliedTwo]  |> normalize

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
          ``Closure test`` ]