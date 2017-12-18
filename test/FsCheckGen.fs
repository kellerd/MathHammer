module FsCheckGen
open GameActions.Primitives
open Expecto
open FsCheck
open State
open Types
type ScalarType = ScalarType of GamePrimitive
type TwoSimilarScalarTypes = TwoSimilarScalarTypes of GamePrimitive * GamePrimitive
type TwoSimilarTypes = TwoSimilarTypes of GamePrimitive * GamePrimitive
type DistType = DistType of GamePrimitive
type ListType = ListType of GamePrimitive
type ListDistScalarType = ScalarGamePrimitive of GamePrimitive | DistGamePrimitive of GamePrimitive | ListGamePrimitive of GamePrimitive
    with member x.ToGamePrimitive() = x |> function | ScalarGamePrimitive g | DistGamePrimitive g | ListGamePrimitive g -> g
let genFloat = Gen.map (fun (NormalFloat f) -> Float f) Arb.generate<_>
let genInt = Gen.map Int Arb.generate<_>
let genStr = Gen.map (fun (NonEmptyString s) -> Str s) Arb.generate<_>
let genNoVal = Gen.constant (Value(NoValue))
let genFOfPrim f = 
    Gen.oneof [ f genFloat;f  genInt;f  genStr; f (Gen.constant NoValue) ]
let genPrimitive = genFOfPrim id
let genNumber f = Gen.oneof [ f genFloat; f genInt ]
let genListOfPrimitive = genFOfPrim (Gen.listOf)  
let genCheck ofType = 
        Gen.oneof [
            Gen.map (Check.Pass) ofType
            Gen.map (Check.Fail) ofType
            // Gen.map2 (fun a b -> Check.Tuple(a,b)) genPrimitive genPrimitive
        ]

let genGpDist gen = 
    let probabilities = Gen.filter (fun f -> f >= 0.0 && f <= 1.0) Arb.generate<_>
    let pair = gen |> Gen.map (fun ls -> List.zip (ls)  <| (Gen.sample (List.length ls) probabilities |> List.ofArray) )
    Gen.map Dist pair        
let genScalarType = Gen.oneof [Gen.map Check (genCheck genPrimitive);(genNumber id)] |> Gen.map ScalarType

let baseTypes g = seq {
        yield g 
        yield Gen.listOf g |> genGpDist
        yield Gen.map Check (genCheck g)
        yield Gen.constant NoValue      
    } 

let similarCombo g = 
    seq { for a in baseTypes g do
                for b in baseTypes g do 
                    yield Gen.zip a b }

let genTwoSimilarTypes =
    let positiveInt = genInt   |> Gen.filter (fun v -> match v with Int i   when i >= 0   -> true | _ -> false)
    let positiveFloat = genFloat |> Gen.filter (fun v -> match v with Float f when f >= 0.0 -> true | _ -> false)
    Seq.collect similarCombo [ positiveInt
                               positiveFloat
                               Gen.listOf positiveInt |> Gen.map (List.map Value >> ParamArray)
                               Gen.listOf positiveFloat |> Gen.map (List.map Value >> ParamArray)]
    |> Gen.oneof
    |> Gen.map TwoSimilarTypes
let genTwoSimilarScalarTypes =
    let positiveInt = genInt   |> Gen.filter (fun v -> match v with Int i   when i >= 0   -> true | _ -> false)
    let positiveFloat = genFloat |> Gen.filter (fun v -> match v with Float f when f >= 0.0 -> true | _ -> false)
    Seq.collect similarCombo [ positiveInt
                               positiveFloat ]
    |> Gen.oneof
    |> Gen.map TwoSimilarScalarTypes

let genDistType = 
    genGpDist genListOfPrimitive |> Gen.map DistType
let genListType = 
    Gen.map (List.map Value >> ParamArray >> ListType) genListOfPrimitive
let genListDistScalarType = 
    let positiveInt = genInt   |> Gen.filter (fun v -> match v with Int i   when i >= 0   -> true | _ -> false)
    let positiveFloat = genFloat |> Gen.filter (fun v -> match v with Float f when f >= 0.0 -> true | _ -> false)
    [baseTypes positiveInt; baseTypes positiveFloat] 
    |> Seq.collect id 
    |> Gen.oneof
    |> Gen.map (function | Dist(d) -> DistGamePrimitive(Dist(d))
                         | ParamArray(ops) -> ListGamePrimitive(ParamArray(ops))
                         | Int _       
                         | Str _       
                         | Float _     
                         | Check _     
                         | NoValue  _  
                         | Tuple _ as g     -> ScalarGamePrimitive g)

 
let genGp = 
    let rec genGp' = function
        | 0 -> genPrimitive
        | _ -> 
            Gen.oneof [
                genPrimitive
                Gen.map Check (genCheck genPrimitive)
                genGpDist genListOfPrimitive
            ] 
    Gen.sized genGp'
let genDie = 
    Gen.oneof [
        Gen.constant D6
        Gen.constant D3
        [1..3] |> Seq.map Gen.constant |> Gen.oneof |> Gen.listOfLength 3 |> Gen.map (fun rs -> Reroll(List.distinct rs, D3))
        [1..6] |> Seq.map Gen.constant |> Gen.oneof |> Gen.listOfLength 6 |> Gen.map (fun rs -> Reroll(List.distinct rs, D6))
    ]

let genOp =
    let rec genOp' = function
        | 0 -> genNoVal
        | n when n>0 -> 
            let subtree = genOp' (n/4)
            let twoParamArrayG = genFOfPrim Gen.two
            let lambda = 
                Gen.map2 (fun (NonEmptyString n) body -> Lam(n, body)) Arb.generate<_> subtree
            Gen.oneof [ 
                Gen.map Value Arb.generate<GamePrimitive> 
                Gen.map2 (fun (NonEmptyString n) op -> PropertyGet(n, op)) Arb.generate<_>  subtree
                Gen.map (ParamArray >> Value) (Gen.listOf subtree)
                Gen.map (fun (NonEmptyString n) -> Var n) Arb.generate<_> 
                Gen.map3 (fun (NonEmptyString n) op value -> App(Lam(n, op),value)) Arb.generate<_> subtree subtree 
                Gen.map3 (fun (NonEmptyString n) value body -> Let(n, value, body)) Arb.generate<_> subtree subtree
                Gen.map3 (fun ifExpr thenExpr elseExpr -> IfThenElse(ifExpr, thenExpr, elseExpr)) subtree subtree (Gen.optionOf subtree)
                Gen.oneof [
                    Gen.map2 (fun lam value -> App(lam,value)) lambda subtree
                    Gen.map  (fun d           -> App(Call(Dice(d)), Value NoValue)) Arb.generate<_>
                    Gen.map  (fun ops         -> App(Call(Product), (ops |> List.map Value |> opList)))  (genNumber (Gen.listOf)) 
                    Gen.map  (fun ops         -> App(Call(Total),   (ops |> List.map Value |> opList)))  (genFOfPrim (Gen.listOf))        
                    Gen.map  (fun ops         -> App(Call(Count),   (ops |> List.map Value |> opList)))  (genFOfPrim (Gen.listOf))   
                    Gen.map  (fun (gp,gp2)    -> App(Call(GreaterThan  ), opList  [Value(gp);Value(gp2)])) twoParamArrayG
                    Gen.map  (fun (gp,gp2)    -> App(Call(Equals       ), opList  [Value(gp);Value(gp2)])) twoParamArrayG
                    Gen.map  (fun (gp,gp2)    -> App(Call(LessThan     ), opList  [Value(gp);Value(gp2)])) twoParamArrayG
                    Gen.map  (fun (gp,gp2)    -> App(Call(NotEquals    ), opList  [Value(gp);Value(gp2)])) twoParamArrayG
                    Gen.map3 (fun name s ints -> App(Call(Repeat), opList ([Lam(name, s);Value(ints)]))) Arb.generate<_> subtree genInt 
                    Gen.map2 (fun op op2      -> App(Call(And),    opList  [op; op2])) subtree subtree
                    Gen.map2 (fun op op2      -> App(Call(Or),     opList  [op; op2])) subtree subtree
                ]                        
                ]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized genOp'
//Gen.sample 1 genOp


type GamePrimitiveGen() = static member GamePrimitive() : Arbitrary<GamePrimitive> = Arb.fromGen genGp
type DistTypeGen() = static member DistType() : Arbitrary<DistType> = genDistType |> Arb.fromGen 
type ListTypeGen() = static member ListType() : Arbitrary<ListType> = genListType |> Arb.fromGen 
type ScalarTypeGen() = static member ScalarType() : Arbitrary<ScalarType> = genScalarType |> Arb.fromGen 
type TwoSimilarTypeGen() = static member TwoSimilarType() : Arbitrary<TwoSimilarTypes> = genTwoSimilarTypes |> Arb.fromGen 
type TwoSimilarScalarTypeGen() = static member TwoSimilarScalarType() : Arbitrary<TwoSimilarScalarTypes> = genTwoSimilarScalarTypes |> Arb.fromGen 
type ListDistScalarTypeGen() = static member ListDistScalarType() : Arbitrary<ListDistScalarType>  = genListDistScalarType |> Arb.fromGen
type DieGen() = static member Die() : Arbitrary<Die> = Arb.fromGen genDie
type GenOp() = static member Operation() : Arbitrary<Operation> = Arb.fromGen genOp
let config = { FsCheckConfig.defaultConfig with 
                    arbitrary =    (typeof<GenOp>) 
                                :: (typeof<DistTypeGen>)
                                :: (typeof<ListTypeGen>)
                                :: (typeof<ScalarTypeGen>)
                                :: (typeof<TwoSimilarTypeGen>)
                                :: (typeof<TwoSimilarScalarTypeGen>)
                                :: (typeof<GamePrimitiveGen>)
                                :: (typeof<ListDistScalarTypeGen>)
                                :: (typeof<DieGen>)
                                ::FsCheckConfig.defaultConfig.arbitrary }