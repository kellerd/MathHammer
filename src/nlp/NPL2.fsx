#r "../../../../../.nuget/packages/ikvm/8.1.5717/lib/IKVM.Runtime.dll"
#r "../../../../../.nuget/packages/ikvm/8.1.5717/lib/IKVM.OpenJDK.Core.dll"
#r "../../../../../.nuget/packages/ikvm/8.1.5717/lib/IKVM.OpenJDK.Text.dll"
#r "../../../../../.nuget/packages/stanford.nlp.parser/3.8.0/lib/ejml-0.23.dll"
#r "../../../../../.nuget/packages/stanford.nlp.parser/3.8.0/lib/slf4j-api.dll"
#r "../../../../../.nuget/packages/stanford.nlp.parser/3.8.0/lib/stanford-parser.dll"
#r "../../../../../.nuget/packages/stanford.nlp.parser.fsharp/0.0.14/lib/Stanford.NLP.Parser.Fsharp.dll"
open edu.stanford.nlp.``process``
open edu.stanford.nlp.parser.lexparser
open java.io
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
let w2sent = new WordToSentenceProcessor()
let getTree question =

    let tokenizer = tlp.getTokenizerFactory().getTokenizer(new java.io.StringReader(question))

    let sentence = tokenizer.tokenize()

    w2sent.``process``(sentence)
    |> Iterable.castToSeq<ArrayList>
    |> Seq.map (parser.apply)

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
   
let mapToWords (words:seq<Word>) = 
    words |> Seq.map (fun n -> n.word()) |> String.concat " "
let getHeadText (node:Tree) = 
    if node.numChildren() = 0 then 
        node.yieldWords()  |> Iterable.castToSeq<Word> |> mapToWords |> Some
    else None
let getAllText (node:Tree) = 
    node.yieldWords() |> Iterable.castToSeq<Word> |> mapToWords
let getChildText (node:Tree) = 
    node.yieldWords() |> Iterable.castToSeq<Word> |> Seq.skip 1 |> mapToWords

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
    | Not
    | Max
    | Min
    | Sub
    | Median
    | Mean
    | Mode
    | Least
    | Largest
    | Suffer
    | FMap

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
    | Ignore  of Tree
    | Cont of original:Tree * (Operation -> Operation)
let findTree = function
    | NodeInfo(Node t,_) -> t
    | NodeInfo(Word (t,_),_) -> t
    | NodeInfo(Cont (t, _),_) -> t
    | NodeInfo(Ignore t, _) -> t

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

let (|TryInteger|_|) op =
    match op with 
    | Value(Str(n)) -> 
        match System.Int32.TryParse(n) with 
        | true, n -> Some n 
        | false, _ -> None
    | _ -> None
let (|TryFloat|_|) op =
    match op with 
    | Value(Str(n)) -> 
        match System.Double.TryParse(n) with 
        | true, n -> Some n 
        | false, _ -> None
    | _ -> None
let (|IsOperation|_|) = fun n -> match n with 
                                       | BasicNode(_, NodeInfo(Word (t, op), _), []) -> Some (t,op) 
                                       |  BasicNode
                                            (Some VBZ,NodeInfo (Word (t,op),_),
                                             [BasicNode (None,_,[])])  -> Some (t,op) 
                                       | _ -> None
let (|AllOperations|_|) children = 
    let check =  List.choose (|IsOperation|_|) children            
    if List.length check = List.length children then Some check
    else None

#load "ParseLibrary.fsx"

type Position = { node:Tree ; queue:Tree seq }
    /// define an initial position
let initTree t = { node= t; queue = t.children()  }
    /// increment the column number
let moveNextPreOrder p = 
    let nextNode = Seq.tryHead p.queue
    nextNode 
    |> Option.map (fun nextNode -> 
        let unvisited = Seq.tail p.queue
        let newUnvisited = nextNode.children()
        { node = nextNode; queue = Seq.append newUnvisited unvisited })

/// increment the line number and set the column to 0
type InputState = {
    position : Position 
}


/// Create a new InputState from a string
let rec fromStr str = 
    getTree str
    |> Seq.map (fun t -> { position = initTree t })
    |> List.ofSeq

let tree = 
    let t  = fromStr "Roll a D6" |> Seq.head
    t.position.node.children().[0]
tree.pennPrint()

/// used by satisfy
let nextNode moveFunction input =
    let label n = getLabel n, getHeadText n
    match moveFunction input.position with 
    | None  -> 
        input, None
    | Some newPosition ->
        {input with position = newPosition}, label newPosition.node |> Some



// // =============================================
// // Standard parsers 
// // =============================================
open ParseLibrary


let onlyChildren (p:Parser<('a * InputState), InputState>) = 
    let label = sprintf "onlyChildrenOf %s" (getLabel p) 
    let innerFn input =
        //Chop off tail of input
        let tail = Seq.tail input.position.queue
        let newQueue = {input with position = {input.position with queue = Seq.head input.position.queue |> Seq.singleton } }

        // run parser1 with the chopped off queue
        let result1 = runOnInput p newQueue
        let updateState newInput = 
            { newInput 
                with position = 
                        { newInput.position 
                            with queue = 
                                 Seq.append 
                                    newInput.position.queue 
                                    tail } }
        // test the result for Failure/Success
        match result1 with
        | Success (result,newInput) -> 
            // if success, return the original result with old stuff back on the queue
            Success(result, updateState newInput) 
        | Failure (a,b,position) -> 
            //Failure  with queued input state put back on
            Failure(a, b, { position with node = updateState position.node })
    // return the inner function
    {parseFn=innerFn; label=label}


type Parsed = 
    | Tag of PennTreebankIITags 
    | Ignored 
    | Op of Operation
let toTag parsed = 
    match parsed with
    | None, Some text -> 
        Str text |> Value |> Op 
    | Some tag, None -> 
        Tag tag
    | None, None -> 
        NoValue |> Value |> Op
    | Some tag, Some text -> 
        Str text |> Value |> Op
let toSafeOp f parsed =  
    match parsed with 
    | None, Some text -> 
        f text |> Op 
    | Some tag, None -> 
        Tag tag
    | None, None -> 
        NoValue |> Value |> Op
    | Some tag, Some text -> 
        Str text |> Value |> Op
let satisfy = ParseLibrary.satisfy (nextNode moveNextPreOrder)

// // ------------------------------
// // string parsing
// // ------------------------------
let pnstr = 
    let label = sprintf "any match "
    let predicate (tag,ch) = (ch = None)
    satisfy predicate label
let panystr = 
    let label = sprintf "any match "
    let predicate (_,ch) = (Option.isSome ch)
    satisfy predicate label
    |>>  (function (_,Some t) -> t | _ -> "")
let ppen tag = 
    let label = sprintf "match: %A " tag
    let predicate (penTag,ch) = (penTag = Some tag && ch = None)
    satisfy predicate label
    |>>  (fst >> Option.defaultValue ROOT)
// /// parse a char 
let pstr str = 
    // label is just the character
    let label = sprintf "%s" str 

    let predicate (_,ch) = (ch = Some str) 
    satisfy predicate label 
    |>>  (function (_,Some t) -> t | _ -> "")

let pany = 
    satisfy (fun _ -> true) "any"

let debug it = 
        it.position.node.pennString().Trim()

let trees = fromStr "Roll a D6" |> Seq.head
let log trees = trees.position.node.pennPrint(); trees
let f = Option.bind moveNextPreOrder
trees |> log
trees.position |> Some |> f |> Option.map (fun n -> n.node.pennPrint())

// let panyUntiltag tag str =  manyUntil (ppen tag) (pnstr |>>  toTag) |>>  List.head >>. pstr str <?> (sprintf "%A : %s" tag str) 
// let panyUntil str = manyUntil (pstr str |>>  (fun s -> None, Some s)) pnstr  |>>  List.head <?> (sprintf ".* %s"  str) 
// let roll = panyUntiltag NNP "Roll"
// let a = panyUntil "a"
// let d6 = panyUntil "D6"
// let sentence = roll .>>. a .>>. d6 <?> "Roll a d6"

let reduceToOperation lst =
    let newList, state = 
        List.mapFold (fun  (tag,prevText) parsed ->
            printfn "Tag: %A, prevText: %A, parsed: %A" tag prevText parsed
            let log (result,(stateTag,newState)) = printfn "=====> Result: %A, NextTag: %A, NextStage: %A" result stateTag newState; (result,(stateTag,newState))
            match parsed with
            | Ignored -> ([], (tag, prevText))
            | Op(Value(Str(s))) -> 
                let space = 
                    match tag with 
                    | Some (SentenceCloser | Comma | EQT | Punctuation ) -> ""
                    | _ -> " "
                match prevText with 
                | Some acc -> ([], (tag, Some (sprintf "%s%s%s" acc space s)))
                | None ->     ([], (tag, Some s))
            | Op(op) ->       ((Option.map(Str >> Value) prevText |> Option.toList) @ [op], (tag, None))
            | Tag(newTag) ->  ([], (Some newTag, prevText))
            |> log
        )  (Some ROOT, None) lst
    match state with 
    | _, Some op -> newList  @ [[Value(Str(op))]]  |> List.collect id
    | _ -> newList |> List.collect id
    |> 
    function 
  | [op] -> op
  | ops -> ops |> ParamArray |> Value

let advance tree = 
    tree.position 
    |> Some 
    |> f 
    |> Option.map (fun p -> {tree with position = p})
    |> Option.defaultValue tree

let pint = 
    let matchInteger = 
        satisfy (function 
               | (_,Some ch) -> System.Int32.TryParse(ch) |> fst 
               | _ -> false) "integer "
        |>> (function 
           | (_,Some s) -> System.Int32.Parse(s) 
           | _ -> 0)
    let combinedMatch = ppen CD >>. matchInteger

    let input = fromStr "12" |> List.head |> advance |> advance 
    let d = input |> debug 
    let p = run combinedMatch input 
    combinedMatch 
let pfloat = 
    let matchFloat = 
        satisfy (function 
               | (_,Some ch) -> System.Double.TryParse(ch) |> fst 
               | _ -> false) "float "
        |>>  (function 
            | (_,Some s) -> System.Double.Parse(s) 
            | _ -> 0.0)
    let combinedMatch = ppen CD >>. matchFloat

    let input = fromStr "12.5" |> List.head |> advance |> advance 
    let d = input |> debug 
    let p = run combinedMatch input 
    combinedMatch   
let toInt = Int >> Value >> Op
let toFloat  = Float >> Value >> Op 
let toStr = Str >> Value >> Op 
let toDice n _ = Op(App(Call Dice, Value(Int n)))
let toVar v _ = Op(Var(v))
let toText t _ = Op(Value(Str(t)))
let toIgnored _ = Ignored
let toOp op  _ = Op(op)

let pdistance =  
    let combinedMatch = pint .>> (ppen NN <|> ppen EQT) .>> pstr "''" |>> (Distance >> Value >> Op)

    let input = [ fromStr "1''" |> List.head |> advance 
                  fromStr "Within 1'' of the " |> List.head |> advance |> advance|> advance|> advance|> advance ]
    let d = input |> List.map debug 
    let p = input |> List.map (run combinedMatch)
    combinedMatch     
let numbers = 
    let combinedMatch = choice [ pdistance 
                                 mapP toInt pint
                                 mapP toFloat pfloat ]
    let input = [fromStr "12''" |> List.head |> advance 
                 fromStr "12\"" |> List.head |> advance 
                 fromStr "12"   |> List.head |> advance |> advance 
                 fromStr "12.5" |> List.head |> advance |> advance ]
    let d = input |> List.map debug 
    let p = input |> List.map (run combinedMatch) 
    combinedMatch
let determiner =  
    choice [
        ppen DT >>. pstr "a"  |>> toOp (Value(Int(1))) 
        ppen DT >>. pstr "each" |>> toOp (Lam("obj", App(Call Count, Value(ParamArray[Var "obj";]))))
        ppen DT |>> toOp (Lam("obj", Var "obj"))
    ]   
let ignoreTag =
    anyOf ppen [ SYM ; NNS; Colon; Comma; EQT; SentenceCloser ] |>> toIgnored
let escapeTag (x,y) = (x, Option.map escape_string y)
let ignored = 
    choice [
        ppen PRP .>> (pstr "You" <|> pstr "you")
        ppen MD .>> pstr "can" 
    ] |>> Parsed.Tag 
let words = 
    let combinedMatch = 
        choice [ ignored
                 determiner 
                 (pany |>> (escapeTag >> toTag)) ]
    let input = [fromStr "12''" |> List.head |> advance  |> advance 
                 fromStr "12"   |> List.head |> advance  |> advance |> advance
                 fromStr "12.5" |> List.head |> advance  |> advance |> advance ]
    let d = input |> List.map debug 
    let p = input |> List.map (run combinedMatch) 
    combinedMatch
let D = 
    let combinedMatch = 
        choice [ ppen NNS
                 ppen NNP
                 ppen NN ] 
        >>. 
        choice [ pstr "D6" <|> pstr "d6" |>> toDice 6
                 pstr "D3" <|> pstr "d3" |>> toDice 3 ] 
    let input = [ fromStr "D6" |> List.head |> advance 
                  fromStr "d6" |> List.head |> advance
                  fromStr "D3" |> List.head |> advance 
                  fromStr "d3" |> List.head |> advance ]
    let d = input |> List.map debug 
    let p = input |> List.map (run combinedMatch) 
    combinedMatch
let variables = 
    choice [
        sequence [ ppen NN |>> Tag; pstr "enemy" |>> toStr ; ppen NN |>> Tag; pstr "unit" |>> toStr ]  |>> toVar "unit"
        ppen NN >>. (pstr "unit" <|> pstr "enemy") |>> toVar "Target"
    ]
let keywords = 
    choice [
        sequence [ ppen JJ |>> toIgnored; pstr "mortal" |>> toIgnored ; ppen NN  |>> toIgnored;  pstr "wound" |>> toIgnored ]  |>> toText "Mortal Wound"
        sequence [ ppen NN |>> toIgnored; pstr "hit"    |>> toIgnored ; ppen NNS |>> toIgnored; (pstr "roll" <|> pstr "rolls") |>> toIgnored  ]  |>> toText  "Hit Roll"
        sequence [ ppen NN |>> toIgnored; pstr "wound"  |>> toIgnored ; ppen NNS |>> toIgnored; (pstr "roll" <|> pstr "rolls") |>> toIgnored  ]  |>> toText  "Wound Roll"
        ppen NN >>. pstr "Fight" .>> ppen NN .>> pstr "phase" |>> (fun phase -> ParamArray[ Value(Str("Phase")); Value (Str phase)] |> Value |> Op)
    ]    
let actions = 
    ppen VBZ >>. pstr "suffers" |>> toOp (Call Suffer)    
let dPlus = 
    let combinedMatch = pint .>> (ppen NNS <|> ppen NNP <|> ppen NN) .>> pstr "+" |>> (gte >> Op)
    let input = [ fromStr "4+" |> List.head |> advance 
                  fromStr "5+" |> List.head |> advance
                  fromStr "7+" |> List.head |> advance 
                  fromStr "+7" |> List.head |> advance ]
    let d = input |> List.map debug 
    let p = input |> List.map (run combinedMatch) 
    combinedMatch
let gamePrimitive = choice [ D; dPlus; numbers; variables; keywords; actions; ignoreTag; words ]
let scannedWords =  many gamePrimitive |>> reduceToOperation
let runP t = t |> List.map (log >> run scannedWords) 
let runAndPrint t = let result = runP t in t |> List.iter (debug >> printfn "%s"); result |> List.iter (printResult debug) 
fromStr "Roll a D6."  |> runAndPrint
fromStr "At the end of the Fight phase, roll a D6 for each enemy unit within 1\" of the Warlord. On a 4+ that unit suffers a mortal wound."  |> runAndPrint
fromStr "Roll a D6 and count the results, then roll another D6 for each success." |> runAndPrint
fromStr "Each time the bearer fights, it can make one (and only one) attack with this weapon. Make D3 hit rolls for this attack instead of one. This is in addition to the bearer’s attacks." |> runAndPrint
fromStr "If the hit result is 6, count the result as two hits, otherwise thr attack fails" |> runAndPrint
fromStr "You can re-roll hit rolls of 1 for this weapon." |> runAndPrint
fromStr "In addition, each time you make a hit roll of 6+ for this weapon, you can make an additional hit roll. These additional hit rolls cannot generate further additional hit rolls." |> runAndPrint
fromStr "Each time you make a wound roll of 6+ for this weapon, the target unit suffers a mortal wound in addition to any other damage." |> runAndPrint


        // let defaultNewOp accOp = 
        //     match tag, parsed with 
        //     | Some CD, Op(TryInteger n) -> Value(Int n) , (tag, None)
        //     | Some CD, Op(TryFloat n)   -> Value(Float n), (tag, None)
        //     | _, Op(newOperation)-> Some([accOp; newOperation;]), (tag, None)
        //     | _, Tag(newTag) -> Some([accOp]), (newTag, None)
        // match parsed, prevText with
        // | Tag (SentenceCloser | Comma | EQT | Punctuation ) , Some(Value(Str(acc)) as accOp) ->
        //     match newOperation with 
        //     | Some(Value(Str(s))) -> 
        //         let s' = sprintf "%s%s" acc s
        //         let newText = Value(Str(s'))
        //         None, (tag, Some newText)
        // | Op(Value(Str(s))), Some(Value(Str(acc))) ->
        //     let s' = sprintf "%s%s" acc s
        //     let newText = Value(Str(s'))
        //     None, (tag, Some newText)
        // | Tag EQT, Some(Value(Int(n))) -> 
        //     None, (Some EQT,Some(Value(Distance(n))))
        //  | _, Some(Value(Int(n)) as accOp) ->
        //     let newDistance = 
        //         Value(Distance(n))
        //     Some([newDistance; defaultNewOp tag newOperation;]), (Some EQT, None)
        // | _, Some(accOp) ->
        //     Some([accOp;defaultNewOp tag newOperation;]), (newTag, None)
        // | Some tag, None -> None, (Some tag, None)
        // | None, None ->
        //     match defaultNewOp tag newOperation with 
        //     | Value(Int(_) | Str(_) as op) ->
        //         None, (tag, Some (Value(op)))
        //     | op ->
        //         Some [op], (tag, None)




// /// Choose any of a list of characters
// let anyOf listOfChars = 
//     let label = sprintf "anyOf %A" listOfChars 
//     listOfChars
//     |> List.map pchar // convert into parsers
//     |> choice
//     <?> label

// /// Convert a list of chars to a string
// let charListToStr charList =
//     String(List.toArray charList) 

// /// Parses a sequence of zero or more chars with the char parser cp. 
// /// It returns the parsed chars as a string.
// let manyChars cp =
//     many cp
//     |>> charListToStr

// /// Parses a sequence of one or more chars with the char parser cp. 
// /// It returns the parsed chars as a string.
// let manyChars1 cp =
//     many1 cp
//     |>> charListToStr

// /// parse a specific string
// let pstring str = 
//     // label is just the string
//     let label = str 

//     str
//     // convert to list of char
//     |> List.ofSeq
//     // map each char to a pchar
//     |> List.map pchar 
//     // convert to Parser<char list>
//     |> sequence
//     // convert Parser<char list> to Parser<string>
//     |>>  charListToStr 
//     <?> label

// // ------------------------------
// // whitespace parsing
// // ------------------------------

// /// parse a whitespace char
// let whitespaceChar = 
//     let predicate = Char.IsWhiteSpace 
//     let label = "whitespace"
//     satisfy predicate label 

// /// parse zero or more whitespace char
// let spaces = many whitespaceChar

// /// parse one or more whitespace char
// let spaces1 = many1 whitespaceChar



// // ------------------------------
// // number parsing
// // ------------------------------

// /// parse a digit
// let digitChar = 
//     let predicate = Char.IsDigit 
//     let label = "digit"
//     satisfy predicate label 


// // parse an integer
// let pint = 
//     let label = "integer" 

//     // helper
//     let resultToInt (sign,digits) = 
//         let i = digits |> int  // ignore int overflow for now
//         match sign with
//         | Some ch -> -i  // negate the int
//         | None -> i
            
//     // define parser for one or more digits
//     let digits = manyChars1 digitChar 

//     // an "int" is optional sign + one or more digits
//     opt (pchar '-') .>>. digits 
//     |>>  resultToInt
//     <?> label

// // parse a float
// let pfloat = 
//     let label = "float" 

//     // helper
//     let resultToFloat (((sign,digits1),point),digits2) = 
//         let fl = sprintf "%s.%s" digits1 digits2 |> float
//         match sign with
//         | Some ch -> -fl  // negate the float
//         | None -> fl
            
//     // define parser for one or more digits 
//     let digits = manyChars1 digitChar 

//     // a float is sign, digits, point, digits (ignore exponents for now)
//     opt (pchar '-') .>>. digits .>>. pchar '.' .>>. digits 
//     |>>  resultToFloat
//     <?> label
