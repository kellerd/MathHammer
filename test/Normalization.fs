module Lambda

open GameActions.Primitives.Types
open MathHammer.Models.State
open GameActions.Primitives.State
open GameActions.Primitives.View
open Expecto

let (==?) actual expected = Expect.equal expected actual ""


let zero = Lam("f",Lam("x", Var("x")))// fun f -> fun x -> x
let one = Lam("f",Lam("x", App(Var "f", Var("x"))))// fun f -> fun x -> f x
let two = Lam( "f", Lam("x",App(Var("f"),App(Var("f"),Var("x"))))) //fun f -> fun x -> f f x
let three = Lam( "f", Lam("x",App(Var("f"),App(Var("f"),App(Var("f"),Var("x")))))) //fun f -> fun x -> f f f x
[<Tests>]
let ``Lambda Calculus`` = 
    let rec doAdding op =
        match op with 
        | App(Value(Str "Add 1"), op2) -> 1 + doAdding op2
        | Value(Str "Zero") -> 0
    let doTest (op,expected) = 
        let result = Value(Str "Zero") |%> (Value(Str "Add 1") |%> op) |> normalizeOp |> doAdding    
        test (sprintf "Lambda for %d" expected) { result ==? expected }
    testList "Lambda Calculus Normalization" (List.map doTest [zero,0;one,1;two,2;three,3])


[<Tests>]
let ``Ski Combinators`` =
    let I = Lam("x", Var "x")
    let K = Lam("x", Lam ("y", Var "x"))
    let xz = App(Var "x", Var"z")
    let yz = App(Var "y", Var "z")
    let ``xz(yz)`` = App(xz,yz)
    let S = Lam("x", Lam ("y", Lam ("z", ``xz(yz)``)))
    
    let F = App(S,K)
    let T = K 
    let NOT = App(F,T)
    let OR = K
    let AND = F

    testList "Ski Combinators" [
        test "I is identity" {
            let x' = vInt 6
            App(I, x') |> normalizeOp ==? x'
        }
        test "K returns first" {
            let x' = vInt 6
            let y' = vInt 8
            App(App(K, x'),y') |> normalizeOp ==? x'
        }        
        testList "Test Boolean Functions" [
            test "F returns second" {
                let x' = vInt 6
                let y' = vInt 8
                App(App(F, x'),y') |> normalizeOp ==? y'          
            }
            test "T returns first" {
                let x' = vInt 6
                let y' = vInt 8
                App(App(T, x'),y') |> normalizeOp ==? x'          
            }
            test "NOT = (SK)(K)" {
                let ``(SK)(K)`` = (App(App(S,K), K)) |> normalizeOp                
                (NOT |> normalizeOp) ==? ``(SK)(K)`` 
            }
            test "True Not == False " {
                (App(T,NOT) |> normalizeOp) ==? (F |> normalizeOp) 
            }
            test "True Not == False " {
                (App(F,NOT) |> normalizeOp) ==? (T |> normalizeOp) 
            }     
            test "x OR y" {
                (App(T,App(OR,T))|> normalizeOp) ==? (T |> normalizeOp)  
                (App(F,App(OR,T))|> normalizeOp) ==? (T |> normalizeOp)  
                (App(T,App(OR,F))|> normalizeOp) ==? (T |> normalizeOp)  
                (App(F,App(OR,F))|> normalizeOp) ==? (F |> normalizeOp)                 
            } 
            test "x AND y" {
                (App(T,App(T,AND))|> normalizeOp) ==? (T |> normalizeOp)  
                (App(F,App(T,AND))|> normalizeOp) ==? (F |> normalizeOp)  
                (App(T,App(F,AND))|> normalizeOp) ==? (F |> normalizeOp)  
                (App(F,App(F,AND))|> normalizeOp) ==? (F |> normalizeOp)                 
            }
        ]
    ]

// let count ops = App(Call Count,ParamArray(ops))
// let count' ops = App(Call Count,App(Call Unfold, ParamArray(ops)))

// let normalizedFirst = count [normalizeOp appliedTwo;normalizeOp appliedTwo;normalizeOp appliedTwo] |> normalizeOp
// let normalizedSecond = count [appliedTwo;appliedTwo;appliedTwo] |> normalizeOp

// normalizedFirst = normalizedSecond

// let normalizedFirst' = count' [normalizeOp appliedTwo;normalizeOp appliedTwo;normalizeOp appliedTwo]|> normalizeOp
// let normalizedSecond' = count'  [appliedTwo;appliedTwo;appliedTwo]  |> normalizeOp

// normalizedFirst' = normalizedSecond'

// let v = Value(Int(3))
// //Let x = 3 returns 3 as well as binding to environment
// let retValueIsSame v f =
//     let evaled = Let("x", v ,Var ("x")) |> f |> evalOp standardCall Map.empty<_,_> 
//     let evaled' = v |> f |> evalOp standardCall Map.empty<_,_> 
//     evaled = evaled'
// retValueIsSame v id
// retValueIsSame v normalizeOp
// //Let x = some number in
// //x + some other number
// let addition x y  =    
//     let v = Value(Int(x))
//     let (Value(Dist(d))) =
//         Let("x", v ,App(Call Total, ParamArray([Value(Int(y));Var ("x")])))
//         |> evalOp standardCall Map.empty<_,_>
//     let expected = Distribution.always (Int(x + y))
//     printfn "Is %A = %A" d expected
//     d = expected
// //Let x = 6
// //Total of x is 6
// let totalOfXIsX x = 
//     let v = Value(Int(x))
//     let (Value(Dist(d))) =
//         Let("x", v ,App(Call Total, v)) 
//         |> evalOp standardCall Map.empty<_,_>
//     let expected = Distribution.always (Int(x))
//     printfn "Is %A = %A" d expected
//     d = expected
// //Count of one passed result is 1
// let countOfOneXIsOneX x = 
//     let v = Value(Check(Check.Pass(Int(x))))
//     let (Value(Dist(d))) =
//         Let("x", v ,App(Call Count, v)) 
//         |> evalOp standardCall Map.empty<_,_>
//     let expected = Distribution.always (Check(Check.Tuple (Int(1),Int(0))))
//     printfn "Is %A = %A" d expected
//     d = expected

// totalOfXIsX 6  
// addition 3 9    
// countOfOneXIsOneX 6  

// let unfoldD6 = 
//     let WS = dPlus D6 3
//     let A = vInt 3
//     let unfold = unfoldOp WS A
//     unfold 
//     |> evalOp standardCall Map.empty<_,_>

// let c = ParamArray[ParamArray[Value (Str "M"); Value(Int 8)]; ParamArray[Value (Str "T"); Value(Float 6.)]]

// PropertyGet("T", c) |> normalizeOp = Value(Float 6.)   
// PropertyGet("M", c) |> normalizeOp = Value(Int 8)                             
// PropertyGet("H", c) |> normalizeOp = PropertyGet("H", c)  