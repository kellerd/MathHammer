#r "../../../../../.nuget/packages/ikvm/8.1.5717/lib/IKVM.Runtime.dll"
open sun.reflect.generics.tree
#r "../../../../../.nuget/packages/ikvm/8.1.5717/lib/IKVM.OpenJDK.Core.dll"
#r "../../../../../.nuget/packages/ikvm/8.1.5717/lib/IKVM.OpenJDK.Text.dll"
#r "../../../../../.nuget/packages/stanford.nlp.parser/3.8.0/lib/ejml-0.23.dll"
#r "../../../../../.nuget/packages/stanford.nlp.parser/3.8.0/lib/slf4j-api.dll"
#r "../../../../../.nuget/packages/stanford.nlp.parser/3.8.0/lib/stanford-parser.dll"
#r "../../../../../.nuget/packages/stanford.nlp.parser.fsharp/0.0.14/lib/Stanford.NLP.Parser.Fsharp.dll"
open edu.stanford.nlp.parser.lexparser
open edu.stanford.nlp.ling
open edu.stanford.nlp.trees
open java.util
open Stanford.NLP.FSharp.Parser
let modelsFolder = __SOURCE_DIRECTORY__ + @"\..\..\paket-files\nlp.stanford.edu\stanford-parser-full-2013-06-20\stanford-parser-3.2.0-models"
let model = modelsFolder + @"\edu\stanford\nlp\models\lexparser\englishPCFG.ser.gz"
let options = [|"-maxLength"; "500";
                "-retainTmpSubcategories";
                "-MAX_ITEMS"; "500000";
                "-outputFormat"; "penn,typedDependenciesCollapsed"|]
let parser = LexicalizedParser.loadModel(model, options)
let tlp = PennTreebankLanguagePack()
let getTree question =
    let tokenizer = tlp.getTokenizerFactory().getTokenizer(new java.io.StringReader(question))
    let sentence = tokenizer.tokenize()
    parser.apply(sentence)
let mapToWords (words:seq<Word>) = 
    words |> Seq.map (fun n -> n.word()) |> String.concat " "
let getHeadText (node:Tree) = 
    node.yieldWords() |> Iterable.castToSeq<Word> |> Seq.take 1 |> mapToWords
let getAllText (node:Tree) = 
    node.yieldWords() |> Iterable.castToSeq<Word> |> mapToWords
let getChildText (node:Tree) = 
    node.yieldWords() |> Iterable.castToSeq<Word> |> Seq.skip 1 |> mapToWords

let (|IsLabeledWith|_|) labels node = 
    labels 
    |> List.tryFind(fun tag -> isLabel tag node)
    |> Option.map (fun _ -> getHeadText node, node.children() |> Array.toList )
let (|IsLabeled|_|) labels node = 
    labels 
    |> List.tryFind(fun tag -> isLabel tag node)
    |> Option.map (fun _ -> getHeadText node)

let rec (|ChildrenLabeled|_|) labels children = 
    //let children = [tree]
    //let labels = [NN;NP] 
    let childTags = children |> Seq.collect(fun (n:Tree) -> n.taggedYield() |> Iterable.castToSeq<TaggedWord> |> Seq.map(fun tw -> tw.tag())) |> Set.ofSeq
    let labelTags = labels |> Seq.map (sprintf "%A") |> Set.ofSeq
    let both = Set.intersect childTags labelTags
    if Set.count both > 0 then 
        children |> Seq.map(getAllText) |> Seq.toList |> Some 
    else None
    
type Probability = double
type Distribution<'a when 'a : equality and 'a : comparison> =
    { Probabilities : ('a * Probability) list }
    

type Check<'a> =
    | Pass of 'a
    | Fail of 'a
    
type GamePrimitive =
    | Int of int
    | Str of string
    | Float of float
    | Check of Check<GamePrimitive>
    | NoValue
    | ParamArray of Operation list
    | Tuple of GamePrimitive * GamePrimitive
    | Dist of Distribution<GamePrimitive>

and Operation =
    | Call of Call
    | PropertyGet of string * Operation
    | Value of GamePrimitive
    | Var of string
    | App of f : Operation * value : Operation
    | Lam of param : string * body : Operation
    | Let of string * value : Operation * body : Operation
    | IfThenElse of ifExpr : Operation * thenExpr : Operation * elseExpr : Operation option
    | Choice of name : string * choices : (string * Operation) list

and Call =
    | Product
    | Division
    | Total
    | Count
    | Repeat
    | Dice
    | GreaterThan
    | Contains
    | Equals
    | NotEquals
    | LessThan
    | ToDist
    | And
    | Or
    | Max
    | Min
    | Sub
    | Median
    | Mean
    | Mode
    | Least
    | Largest
    | Suffer

let tree =  
    "At the end of the Fight phase, roll a D6 for each enemy unit within 1\" of the Warlord. On a 4+ that unit suffers a mortal wound." 
    |> getTree
// [tree]
// |> function | ChildrenLabeled [NN] n -> n
type WordScan = 
    | Tree of PennTreebankIITags option * WordScanNode * WordScan list
and WordScanNode = 
    | Node of Tree
    | Word of Operation 
let word op = 
    Word(op)    

let gt plusValue =
    App(Call GreaterThan, Value(ParamArray [ Var "roll"; Value(Int(plusValue))]))
let eq plusValue =
    App(Call Equals, Value(ParamArray [ Var "roll"; Value(Int(plusValue))]))
let gte plusValue =
    let gte = App(Call Or, Value(ParamArray [ Var "eq"; Var "gt" ]))
    Lam("roll", Let("gt", gt plusValue, Let("eq", eq plusValue, gte)))
    
let (|DValue|_|) s = 
    match s with 
    | "1"  -> Some 1
    | "2"  -> Some 2
    | "3"  -> Some 3
    | "4"  -> Some 4
    | "5"  -> Some 5
    | "6"  -> Some 6
    | _ -> None

let (|TryInteger|_|) n = 
    match System.Int32.TryParse(n) with 
    | true, n -> Some n 
    | false, _ -> None
let (|TryFloat|_|) n = 
    match System.Double.TryParse(n) with 
    | true, n -> Some n 
    | false, _ -> None
let (|AnyContains|_|) words check = 
    List.windowed (List.length words) check 
    |> List.tryFind ((=) words)

let convertNode tree =     
    match tree with 
    | IsLabeled [NN] "D6" -> App(Call Dice, Value(Int 6)) |> word
    | IsLabeled [NN] "D3" -> App(Call Dice, Value(Int 3)) |> word
    | IsLabeled [NN] "enemy" -> Var("Target") |> word
    | IsLabeledWith [NP] (_, ChildrenLabeled [JJ;NN] (AnyContains ["mortal";"wound"] _))  -> ParamArray[ Value(Str("Mortal Wound")); Value (Int 1)] |> Value |> word
    | IsLabeledWith [NP] (_, ChildrenLabeled [JJ;NN] (AnyContains ["Fight"; "phase"] _))  -> ParamArray[ Value(Str("Phase")); Value (Str "Fight")] |> Value |> word
    | IsLabeledWith [NP] (_, ChildrenLabeled [CD;NNS] [_; DValue n;"+"])  -> gte n |> word
    | IsLabeled [CD] (TryInteger n) -> Value(Int n) |> word
    | IsLabeled [CD] (TryFloat n) -> Value(Float n) |> word
    | IsLabeled [VBZ] "suffers" -> Call Suffer |> word
    | _ -> Node tree 
let scanWords (tree:Tree) =     
    tree.pennPrint();
    let rec mapTree (tree:Tree) = 
        let wsn = convertNode tree
        let nodes = tree.getChildrenAsList() |> Iterable.castToSeq<Tree> |> Seq.map mapTree |> Seq.toList
        Tree(getLabel tree, wsn, nodes)
    mapTree tree    
let scanPhrases (tree:WordScan) = 
    let rec foldTree acc tree = 
        match tree with 
        | Tree(_, Word op, _) -> 
            // let children' = 
            //     children
            //     |> Seq.fold foldTree acc    
            op :: acc   
        | Tree(Some (SYM | NNS | Punctuation), Node op, children) ->
            let children' = 
                children
                |> Seq.fold foldTree acc   
            match children' with 
            | Value(Str text)::moreChildren -> (sprintf "%s%s" text (getHeadText op) |> Str |> Value) :: moreChildren
            | moreChildren -> ((getHeadText op) |> Str |> Value) :: moreChildren
        | Tree(Some WordLevel, Node op, children) ->    
            let children' = 
                children
                |> Seq.fold foldTree acc   
            match children' with 
            | Value(Str text)::moreChildren -> (sprintf "%s %s" text (getHeadText op) |> Str |> Value) :: moreChildren
            | moreChildren -> ((getHeadText op) |> Str |> Value) :: moreChildren
        | Tree(_, Node op, children) ->
                 children
                 |> Seq.fold foldTree acc
        | _ -> acc        
        // match getLabel node with 
        // | Some (PhraseLevel as tag) ->
        //     match node with 
        //     | IsLabeledWith [VP] (_, ChildrenLabeled [VB] text) when getHeadText node = "roll" 
    foldTree [] tree   
    |> List.rev

tree
|> scanWords 
|> scanPhrases

Seq.scan
let getGameOperation (tree:Tree) =
    let node = tree
    tree.pennPrint()
    let rec newAcc level acc (node:Tree) =
        node.getChildrenAsList() 
        |> Iterable.castToSeq<Tree>
        |> Seq.fold (foldTree (level+1)) acc   
    and  foldTree level acc (node:Tree) =
        let nodeText = getHeadText node   
        match getLabel node with 
        | Some (SYM | NNS | Punctuation)-> 
            match newAcc level acc node with 
            | Value(Str text)::newAcc -> (text + nodeText |> Str |> Value) :: newAcc
            | newAcc -> (nodeText |> Str |> Value)::newAcc
        | Some (WordLevel as tag) -> 
            printfn "Word: %A - Level %d - %s  - ACC: %A" tag level nodeText newAcc
            match node with 
            | IsLabeled [NN] "D6" -> (Var("D6")) :: newAcc level acc node
            | _ -> 
                match newAcc level acc node with 
                | Value(Str text)::newAcc -> (text + " " + nodeText |> Str |> Value) :: newAcc
                | newAcc -> (nodeText |> Str |> Value) :: newAcc
        // | Some (PhraseLevel as tag) -> 
        //     let nodeText = getHeadText node  
        //     match node with 
        //     // | IsLabeledWith [NP] (_, ChildrenLabeled [DT; NN] ["a"; "D6"]) -> 
        //     //     printfn "Phrase: %A - Level %d - %s - ACC: %A" tag level nodeText acc
        //     //     Var("D6") :: acc
        //     | IsLabeledWith [VP] (_, ChildrenLabeled [VB] text) when getHeadText node = "roll" ->
        //         printfn "Phrase: %A - Level %d - %s - ACC: %A" tag level nodeText acc
        //         let text = node.children() |> Array.skip 1 |> Array.collect(fun n -> n.children()) |> Array.rev |> Array.head |> getAllText
        //         Let(text, newAcc |> List.rev |> List.skip 1 |> ParamArray |> Value, Value(NoValue)) :: acc               
        //     | _ -> newAcc level acc node 
        | Some tag -> 
            //printfn "Unknown: %A - %s " tag nodeText
            newAcc level acc node
        | None -> 
            //printfn "No match: %A - %s " (node.taggedYield() |> Iterable.castToSeq<TaggedWord> |> Seq.map(fun tw -> tw.tag()) |> Seq.tryHead) nodeText
            newAcc      
    match foldTree 0 [] tree with 
    | []  -> NoValue |> Value
    | [x] -> x 
    | xs  -> xs |> List.rev |> ParamArray |> Value

getGameOperation tree |> printfn "%A"



tree.taggedYield() |> Iterable.castToSeq<TaggedWord> |> Seq.map(fun tw -> tw.tag())
tree.``yield``() |> Iterable.castToSeq<CoreLabel> |> Seq.iter(fun n -> printfn "%s" (n.word()))
    
let getKeyPhrases (tree:Tree) =
    let rec foldTree acc (node:Tree) =
        let acc =
            if node.isLeaf() then acc
            else node.getChildrenAsList()
                 |> Iterable.castToSeq<Tree>
                 |> Seq.fold foldTree acc
        match node with 
        | IsLabeledWith [NP] (text, ChildrenLabeled [NN;NNS;NNP;NNPS] childtext ) -> node :: acc
        | _ -> acc      


    let isNNx =  function
        | Label NN | Label NNS
        | Label NNP | Label NNPS -> true
        | _ -> false
    let isNPwithNNx = function
        | Label NP as node ->
            node.getChildrenAsList()
            |> Iterable.castToSeq<Tree>
            |> Seq.exists isNNx
        | _ -> false
    let rec foldTree acc (node:Tree) =
        let acc =
            if node.isLeaf() then acc
            else node.getChildrenAsList()
                 |> Iterable.castToSeq<Tree>
                 |> Seq.fold foldTree acc
        if isNPwithNNx node
          then node :: acc
          else acc
    foldTree [] tree









    tree.pennPrint()
    foldTree [] tree    

getKeyPhrases tree

// let getKeyPhrases (tree:Tree) =
//     tree.pennPrint()
//     let isNNx =  function
//         | Label NN | Label NNS
//         | Label NNP | Label NNPS -> true
//         | _ -> false
//     let isNPwithNNx = function
//         | Label NP as node ->
//             node.getChildrenAsList()
//             |> Iterable.castToSeq<Tree>
//             |> Seq.exists isNNx
//         | _ -> false
//     let rec foldTree acc (node:Tree) =
//         let acc =
//             if node.isLeaf() then acc
//             else node.getChildrenAsList()
//                  |> Iterable.castToSeq<Tree>
//                  |> Seq.fold foldTree acc
//         if isNPwithNNx node
//           then node :: acc
//           else acc
//     foldTree [] tree

let escape_string (str : string) =
   let buf = System.Text.StringBuilder(str.Length)
   let replaceOrLeave c =
      match c with
      | '\r' -> buf.Append "\\r"
      | '\n' -> buf.Append "\\n"
      | '\t' -> buf.Append "\\t"
      | '\\' -> buf.Append "\\\\"
      | '"' -> buf.Append "\\\""
      | _ -> buf.Append c
   str.ToCharArray() |> Array.iter (replaceOrLeave >> ignore)
   buf.ToString()
   
let nlpRule (s:string) = s |> escape_string     

let questions =
    [|" When this unit manifests the Smite psychic power, it affects the closest visible enemy unit within 24\", instead of within 18\". In addition, it inflicts an additional D3 mortal wounds on that enemy unit if this unit contains 4 or 5 Zoanthropes, or
an additional 3 mortal wounds if it contains 6 Zoanthropes."
      "When manifesting or denying a psychic power with a Zoanthrope unit, first select a model in the unit – measure range, visibility etc. from this model. If this unit suffers Perils of the Warp, it suffers D3 mortal wounds as described in the core rules, but units within 6\" will only suffer damage if the Perils of the Warp causes the last model in the Zoanthrope unit to be slain."
      "You can re-roll failed charge rolls for units with this adaptation."
      "You can re-roll wound rolls of 1 in the Fight phase for units with this adaptation."
      "Use this Stratagem at the end of the Fight phase. Select a TYRANIDS unit from your army – that unit can immediately fight again."
      "If a rule requires you to roll a D3, roll a D6 and halve the result." |]
// let doq question =     
//     printfn "Question : %s" question
//     question
//     |> getTree
//     |> 
//     getKeyPhrases tree
//     |> List.rev
//     |> List.iter (fun p ->
//         p.getLeaves()
//         |> Iterable.castToArray<Tree>
//         |> Array.map(fun x-> x.label().value())
//         |> printfn "\t%A")
// questions
// |> Seq.skip 5
// |> Seq.iter doq 
