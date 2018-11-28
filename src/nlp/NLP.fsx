﻿#r "../../../../../.nuget/packages/ikvm/8.1.5717/lib/IKVM.Runtime.dll"
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
        children |> Some 
    else None
    
type Probability = double
type Distribution<'a when 'a : equality and 'a : comparison> =
    { Probabilities : ('a * Probability) list }
    

type Check<'a> =
    | Pass of 'a
    | Fail of 'a
    
type GamePrimitive =
    | Int of int
    | Distance of int
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


// [tree]
// |> function | ChildrenLabeled [NN] n -> n
type Tree<'ITag, 'INodeData> =
    | Empty of 'ITag
    | BasicNode of 'ITag * 'INodeData * Tree<'ITag,'INodeData> list
    | Assignment of 'ITag * label:string * value:Tree<'ITag,'INodeData> list * inExpr:Tree<'ITag,'INodeData> list 
    | IfThenElseBranch of 'ITag * test:Tree<'ITag,'INodeData> list * thenExpr:Tree<'ITag,'INodeData> list * elseExpr:Tree<'ITag,'INodeData> list option
type NodeInfo<'a> = NodeInfo of 'a * int
type Tag = PennTreebankIITags option
type WordScanNode = 
    | Node of Tree
    | Word of original:Tree * Operation 
    | Cont of original:Tree * (Operation -> Operation)

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
let (|IsOperation|_|) = fun n -> match n with 
                                       | BasicNode(_, NodeInfo(Word (_, op), _), []) -> Some op 
                                       | _ -> None
let (|AllOperations|_|) children = 
    let check =  List.choose (|IsOperation|_|) children            
    if List.length check = List.length children then Some check
    else None
let (|Tagged|_|) t tree = 
    let tag = 
        match tree with 
        | Empty tag -> tag
        | BasicNode(tag, _, _) -> tag
        | Assignment(tag, _, _, _) -> tag
        | IfThenElseBranch(tag, _, _, _) -> tag
    if t = tag then Some tree else None 
let (|AllTagged|_|) tags nodes = 
    let matched = 
        Seq.zip tags nodes 
        |> Seq.choose(fun (tag, n) -> match n with Tagged(Some tag) n -> Some n | _ -> None)
    if Seq.isEmpty matched then None
    else Seq.toList matched |> Some
let scanWords (tree:Tree) =   
    let (|Siblings|_|) labels check (node:Tree) = 
        // let children = [tree]
        // //let labels = [NN;NN] 
        // let check =  [(=) "Fight";(=) "phase"]
        // let chillins = children |> Seq.collect(fun n -> n.children()) |> Seq.collect(fun n -> n.children()) |> Seq.toList
        // let parent = chillins.[0].children().[1].children().[1].children().[1]
        // let node = parent.children().[1]
        // getHeadText node
        // getAllText parent
        match node.parent tree with
        | null -> None 
        | parent -> 
            let rightSiblings = parent.children() |> Seq.skipWhile(fun n -> n.equals(node) |> not) |> Seq.toList
            // let siblings = node.siblings root
            // rightSiblings  |> Seq.map (getHeadText)
            // rightSiblings  |> Seq.map (getLabel)
            match rightSiblings with 
            | [] -> None
            | rightSiblings ->
                let combined = Seq.zip3 rightSiblings labels check 
                let both = 
                    combined
                    |> Seq.filter (fun (n, l, c) -> getLabel n = Some l && getHeadText n |> c)
                if Seq.length labels = Seq.length both then 
                    Seq.length labels - 1 |> Some 
                else None
    let always (_:string) = true
    let dvalue = (|DValue|_|) >> Option.isSome
    let convertNode node =     
        let word op = Word(node, op) 
        match node with 
        | IsLabeled [EQT] _ -> Distance 0 |> Value |> word , 0        
        | IsLabeled [NN] ("D6"|"d6") -> App(Call Dice, Value(Int 6)) |> word, 0
        | IsLabeled [NN] ("D3"|"d3") -> App(Call Dice, Value(Int 3)) |> word, 0
        | IsLabeled [NN] ("enemy" | "unit") -> Var("Target") |> word, 0
        | Siblings [JJ;NN]  [(=) "mortal";(=) "wound"] skip -> "Mortal Wound" |> Str |> Value |> word, skip
        | Siblings [NN;NN]  [(=) "enemy";(=) "unit"] skip   -> Var("unit") |> word, skip
        | Siblings [NN;NN]  [(=) "Fight";(=) "phase"] skip  -> (ParamArray[ Value(Str("Phase")); Value (Str "Fight")] |> Value |> word) , skip
        | Siblings [CD;NNS]  [dvalue;(=) "+"] skip as n -> n |> getHeadText |> int |> gte |> word, skip
        | IsLabeled [CD] (TryInteger n) -> Value(Int n) |> word,0
        | IsLabeled [CD] (TryFloat n)   -> Value(Float n) |> word,0
        | IsLabeled [DT] "a"      -> Value(Int(1)) |> word,0
        | IsLabeled [DT] "each"   -> Lam("obj", App(Call Count, Value(ParamArray[Var "obj";]))) |> word,0
        | IsLabeled [DT] _     -> Lam("obj", Var "obj") |> word,0
        | IsLabeled [VBZ] "suffers"     -> Call Suffer |> word,0
        | _ -> Node node,0   
    tree.pennPrint();
    let rec mapTree (node:Tree) = 
        let (wsn, skip) = convertNode node
        let nodes = 
            node.getChildrenAsList() |> Iterable.castToSeq<Tree> |> Seq.map mapTree |> Seq.toList
        let nodeInfo = NodeInfo(wsn, skip)        
        BasicNode (getLabel node, nodeInfo, nodes)
    mapTree tree    
let rec cata fEmpty fNode fAssign ifte (tree:Tree<'ITag, 'INodeData>) = 
    let recurse = cata fEmpty fNode fAssign ifte
    match tree with
    | Empty tag -> 
         fEmpty tag
    | BasicNode (tag, nodeInfo,subtrees) -> 
        fNode tag nodeInfo (subtrees |> Seq.map recurse)
    | Assignment(tag, label, value, inExpr) -> 
        fAssign tag label (value |> Seq.map recurse) (inExpr |> Seq.map recurse)
    | IfThenElseBranch(tag, test, thenExpr, elseExpr) -> 
        ifte tag (test |> Seq.map recurse) (thenExpr |> Seq.map recurse) (elseExpr |> Option.map (Seq.map recurse))
let rec fold fEmpty fNode fAssign ifte acc (tree:Tree<'ITag, 'INodeData>) :'r = 
    let recurse = fold fEmpty fNode fAssign ifte
    match tree with
    | Empty tag -> 
        fEmpty tag acc 
    | BasicNode (tag, nodeInfo,subtrees) -> 
        // determine the local accumulator at this level
        let localAccum = fNode tag acc nodeInfo
        // thread the local accumulator through all the subitems using Seq.fold
        let finalAccum = subtrees |> Seq.fold recurse localAccum 
        // ... and return it
        finalAccum 
    | Assignment(tag, label, value, inExpr) -> 
        let localAccum = fAssign tag acc label    
        let vAccum = value |> Seq.fold recurse localAccum
        let finalAccum = inExpr |> Seq.fold recurse vAccum 
        finalAccum
    | IfThenElseBranch(tag, test, thenExpr, elseExpr) -> 
        let localAccum = ifte tag acc    
        let tAccum = test |> Seq.fold recurse localAccum
        let thenAccum = thenExpr |> Seq.fold recurse localAccum 
        match elseExpr with 
        | Some elseExpr -> 
            elseExpr |> Seq.fold recurse thenAccum
        | None -> 
            thenAccum     


let rec foldBack fEmpty fNode fAssign ifte (tree:Tree<'ITag, 'INodeData>) acc : 'r = 
    let recurse = foldBack fEmpty fNode fAssign  ifte
    match tree with
    | Empty tag -> 
        fEmpty tag acc 
    | BasicNode (tag, nodeInfo,subtrees) -> 
        let childAccum =  Seq.foldBack recurse subtrees acc 
        // determine the local accumulator at this level
        let finalAccum = fNode tag childAccum nodeInfo
        // thread the local accumulator through all the subitems using Seq.fold
        // ... and return it
        finalAccum
    | Assignment(tag, label, value, inExpr) -> 
        let vAccum = Seq.foldBack recurse value acc
        let expr = Seq.foldBack recurse inExpr acc 
        fAssign tag label vAccum expr
    | IfThenElseBranch(tag, test, thenExpr, elseExpr) -> 
        let vAccum = Seq.foldBack recurse test acc
        let expr = Seq.foldBack recurse thenExpr acc 
        match elseExpr with 
        | Some elseExpr -> 
            let elseExpr = Seq.foldBack recurse thenExpr acc 
            ifte tag vAccum expr (Some elseExpr)
        | None -> 
            ifte tag vAccum expr None        
                
let skipChildren children = 
    Seq.fold(fun (num,acc) t -> 
                if num > 0 then num - 1, acc
                else match t with 
                     | Empty _ -> num, acc
                     | BasicNode(_, NodeInfo(_, skip),_) -> skip, t::acc
                     | Assignment _ -> (num, t::acc) 
                     | IfThenElseBranch _ -> num, t::acc) (0,[]) children |> snd |> Seq.rev |> Seq.toList 

let (|AsText|_|) t n = 
    match t, n with 
    | None,              BasicNode (_, NodeInfo (Word (_, Value(Str label)),_),_) -> Some label
    | None,              BasicNode (_, NodeInfo (Word (_, Var label),_),_) -> Some label
    | None,              BasicNode (_, NodeInfo (Word (label, _),_),_) 
    | Some _,  Tagged t (BasicNode (_, NodeInfo (Word (label, _),_),_))
    | None,              BasicNode(_, NodeInfo(Node label, _),_) 
    | Some _,  Tagged t (BasicNode(_, NodeInfo(Node label, _),_) )-> getHeadText label |> Some
    | _ -> None
let (|AllWords|_|) n = 
    let textOnly = 
        List.choose (function AsText None t -> Some t | _ -> None) n
    if List.length textOnly = List.length n then 
        String.concat " " textOnly  |> Some
    else 
        None
let (|IsD6Plus|_|) = function 
    BasicNode(Some NP, 
              NodeInfo(Word(_, App (Call Repeat, 
                                    Value (ParamArray [Value (Int _);
                                                       Lam ("roll",
                                                            Let ("gt", App (Call GreaterThan, Value (ParamArray [Var "roll"; Value (Int _)])),
                                                               Let ("eq", App (Call Equals, Value(ParamArray [Var "roll"; Value (Int _)])),
                                                                   App (Call Or, Value (ParamArray [Var "eq"; Var "gt"])))))]))),_), []) as n -> Some n
        

    | _ -> None                     
let (|IsDice|_|) n = 
    match n with
    | BasicNode(Some NP,  
                NodeInfo(Word(_, App (Call Repeat, 
                                      Value (ParamArray [Value (Int _); 
                                                         App (Call Dice,Value (Int _))]))),_),_) as n  -> Some n
    | _ -> None
let scanPhrases (tree:Tree<Tag, NodeInfo<WordScanNode>>) : Tree<Tag, NodeInfo<WordScanNode>> =
    // let node = 
    //     ("""At the end of the Fight phase, roll a 
    //       D6 for each enemy unit within 1\" of the Warlord. 
    //       On a 4+ that unit suffers a mortal wound."""  |> getTree ).children().[0].children().[2] |> Node
    let fEmpty tag = Empty tag
    let fNode penTags (NodeInfo(node, skip) as n) children = 
        // let node' = 
        //     ("""At the end of the Fight phase, roll a 
        //       D6 for each enemy unit within 1" of the Warlord. 
        //       On a 4+ that unit suffers a mortal wound."""  |> getTree ).children().[0].children().[2]
        // let node = Node node'      
        // let children = node'.children() |> Array.map scanWords |> Array.toSeq
        // let n = (NodeInfo(node, 0))
        // let penTags = Some VP
        // let skip = 0
        let children' = children |> skipChildren
        match node with 
        | Cont _ -> BasicNode(penTags, n, children')  
        | Word _ -> BasicNode(penTags, n, children')  
        | Node op -> 
            match penTags with 
            | Some (SYM | NNS | Punctuation | EQT) -> 
                // let word = (getHeadText op) |> Str |> Value |> Word
                // BasicNode(NodeInfo(penTags, word, skip), children')
                fEmpty penTags
            | Some WordLevel -> 
                let word = Word(op, (getHeadText op) |> Str |> Value)
                BasicNode(penTags, NodeInfo(word, skip), children')
            | Some NP -> 
                match children' with 
                | BasicNode(Some DT, NodeInfo( Word (original,op), skip),_) :: _ when getHeadText original = "each" ->  BasicNode(penTags, n, children') 
                | BasicNode(Some DT, NodeInfo( Word (original,op), skip),_) :: AllOperations(moreChildren) -> 
                    if List.length moreChildren = 1 then 
                        BasicNode(penTags, NodeInfo(Word(original, App(Call Repeat, Value(ParamArray[op; List.head moreChildren]))), skip), []) 
                    else BasicNode(penTags, NodeInfo(Word(original, App(op, Value(ParamArray(moreChildren)))), skip), [])  
                | _ -> BasicNode(penTags, n, children') 
            | Some SBAR ->
                match children' with 
                | AsText (Some IN) "For" :: 
                     BasicNode(Some S, n, (BasicNode(Some NP, np,  AsText (Some DT) "each" :: AllWords label) ) :: action) :: rest -> 
                    let labelText =  label
                    let for' = fun op -> App(Call Repeat, Value(ParamArray[Lam(labelText, op); Var labelText]))
                    let children'' = BasicNode(Some S, n, action) :: rest
                    BasicNode(penTags, NodeInfo(Cont (op, for'),skip), children'')                            
                | _ -> BasicNode(penTags, n, children')                 
            | Some VP -> 
                match children' with 
                | AllTagged [VB;S;SBAR] [AsText (Some VB) ("roll" as t); v; inExpr] -> 
                    Assignment(penTags, t, [v], [inExpr])
                | AllTagged [VBP;NP;PP;SBAR] [ AsText (Some VBP) ("roll" as t)
                                               IsDice dieRoll
                                               BasicNode (Some PP, _,  [AsText (Some IN) ("on"); IsD6Plus (dplus)])
                                               thenExpr ] ->
                    let ifThenOp = IfThenElseBranch(Some PP, [dplus], [thenExpr], None)
                    Assignment(penTags, t, [dieRoll], [ifThenOp])
                | xs ->  
                    List.iter(function BasicNode(Some tag,_,_) -> tag |> printfn "%A"| _ -> printfn "Nothing") xs
                    BasicNode(penTags, n, children')  
            | None -> fEmpty penTags
            | _ -> BasicNode(penTags, n, children')
    let fAssign tag label child inExpr =
        Assignment(tag,label,Seq.toList child,Seq.toList inExpr)  
    let ifte tag test thenExpr elseExpr =
        IfThenElseBranch(tag,Seq.toList  test,Seq.toList  thenExpr,Option.map Seq.toList elseExpr)        
    cata fEmpty fNode fAssign ifte tree 
let foldToOperation (tree:Tree<Tag, NodeInfo<WordScanNode>>) = 
    let fEmpty tag acc = acc
    let (|EndOfPhase|_|) (tag,acc) node = 
        match node, acc with
        | Word (_, Value(Str("At"|"at"))), App (Call Repeat, Value (ParamArray [Lam ("obj",Var "obj"); Value (Str "end")])) :: Value (Str "of") :: App (Call Repeat, Value (ParamArray [Lam ("obj",Var "obj"); Value (ParamArray [Value (Str "Phase"); Value (Str phase)])])) :: rest -> 
            (phase,rest) |> Some 
        | _ -> None        

    let fNode pennTags (tag,acc) (NodeInfo(node, skip) as n)  = 
        match node with
        | EndOfPhase (tag, acc) (phase,rest) -> 
            pennTags, [IfThenElse (App(Call Equals, Value(ParamArray[Var "Phase"; Value(Str(phase))])), Value(ParamArray rest), None)] 
        | Word (_, (Value(Str(s)) as op) ) ->
            match tag, acc with 
            | (Some SentenceCloser | Some Comma | Some EQT | Some Punctuation), Value(Str(acc))::rest -> 
                let s' = sprintf "%s%s" s acc
                pennTags, Value(Str(s')) :: rest
            | _, Value(Str(acc))::rest -> 
                let s' = sprintf "%s %s" s acc
                pennTags, Value(Str(s')) :: rest
            | _ -> pennTags, op:: acc
        | Word (_, (Value(Int(n)) as op)) when tag = Some EQT -> //EQT 
            match acc with 
            |  Value(Distance(0)) :: rest when tag = Some EQT ->  pennTags, Value(Distance(n)) :: rest
            | _ ->  pennTags, op :: acc
        | Word (_,op) ->  pennTags, op :: acc
        | Node n ->  pennTags, acc
        | Cont(_, cont) -> 
            match acc with 
            | [] -> pennTags, [cont (Value NoValue)] 
            | [item] -> pennTags, [cont item] 
            | _ :: _ -> pennTags, [cont (Value(ParamArray(acc)))]
            // match penTags with 
            // | Some WordLevel ->  
            //     penTags, acc
            // | Some penTags -> Some penTags, acc
            // | None -> penTags, acc
    let fAssign tag label (_, vAccum) (_, inExpr) = 
        tag, [Let(label, ParamArray vAccum |> Value, ParamArray inExpr |> Value)]    
    let ifte tag (_, test) (_,thenExpr) elseExpr = 
        tag, [IfThenElse(ParamArray test |> Value,ParamArray thenExpr  |> Value, elseExpr |> Option.map (snd >> ParamArray >> Value))]     
    foldBack fEmpty fNode fAssign ifte tree (None, [])
let tree =  
    //"At the end of the Fight phase, roll a D6 for each enemy unit within 1\" of the Warlord. On a 4+ that unit suffers a mortal wound." 
    //"Roll a D6 and count the results, then roll another D6 for each success."
    "For each unit roll a d6, on a 4+ that unit suffers a mortal wound"
    //"If the hit result is 6, count the result as two hits, otherwise thr attack fails"
    |> getTree
tree
|> scanWords 
|> scanPhrases
|> foldToOperation
|> ignore

let head = tree.children() |> Seq.collect(fun tree -> tree.children()) |> Seq.head
let children = head.children() |> Seq.map (fun c -> c.siblings(tree) |> Iterable.castToSeq<Tree> |> Seq.map getHeadText |> Seq.toList) |> Seq.toList

tree.taggedYield() |> Iterable.castToSeq<TaggedWord> |> Seq.map(fun tw -> tw.tag())
tree.``yield``() |> Iterable.castToSeq<CoreLabel> |> Seq.iter(fun n -> printfn "%s" (n.word()))

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
    [ " When this unit manifests the Smite psychic power, it affects the closest visible enemy unit within 24\", instead of within 18\". In addition, it inflicts an additional D3 mortal wounds on that enemy unit if this unit contains 4 or 5 Zoanthropes, or
an additional 3 mortal wounds if it contains 6 Zoanthropes."
      "When manifesting or denying a psychic power with a Zoanthrope unit, first select a model in the unit – measure range, visibility etc. from this model. If this unit suffers Perils of the Warp, it suffers D3 mortal wounds as described in the core rules, but units within 6\" will only suffer damage if the Perils of the Warp causes the last model in the Zoanthrope unit to be slain."
      "You can re-roll failed charge rolls for units with this adaptation."
      "You can re-roll wound rolls of 1 in the Fight phase for units with this adaptation."
      "Use this Stratagem at the end of the Fight phase. Select a TYRANIDS unit from your army – that unit can immediately fight again."
      "If a rule requires you to roll a D3, roll a D6 and halve the result." ]

questions 
|> List.map (
       getTree
    >> scanWords 
    >> scanPhrases
    >> foldToOperation
)