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

let (|TryInteger|_|) n = 
    match System.Int32.TryParse(n) with 
    | true, n -> Some n 
    | false, _ -> None
let (|TryFloat|_|) n = 
    match System.Double.TryParse(n) with 
    | true, n -> Some n 
    | false, _ -> None
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
let moveNext p = 
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
let rec fromTree str = 
    getTree str
    |> Seq.map (fun t -> { position = initTree t })

let tree = 
    let t  = fromTree "Roll a D6" |> Seq.head
    t.position.node.children().[0]
tree.pennPrint()

/// used by satisfy
let nextNode input =
    // three cases
    // 1) if line >= maxLine -> 
    //       return EOF
    // 2) if col less than line length -> 
    //       return char at colPos, increment colPos
    // 3) if col at line length -> 
    //       return NewLine, increment linePos
    let label n = getLabel n, getHeadText n
    match moveNext input.position with 
    | None  -> 
        input, None
    | Some newPosition ->
        {input with position = newPosition}, label newPosition.node |> Some
    // if linePos >= input.lines.Length then
    //     input, None
    // else
    //     let currentLine = currentLine input
    //     if colPos < currentLine.Length then
    //         let char = currentLine.[colPos]
    //         let newPos = incrCol input.position 
    //         let newState = {input with position=newPos}
    //         newState, Some char
    //     else 
    //         // end of line, so return LF and move to next line
    //         let char = '\n'
    //         let newPos = incrLine input.position 
    //         let newState = {input with position=newPos}
    //         newState, Some char


// // =============================================
// // Standard parsers 
// // =============================================

let satisfy = ParseLibrary.satisfy nextNode

// // ------------------------------
// // string parsing
// // ------------------------------
let pnstr = 
    let label = sprintf "any match "
    let predicate (_,ch) = (ch = None)
    satisfy predicate label
let ppen tag = 
    let label = sprintf "match: %A " tag
    let predicate (penTag,ch) = (penTag = Some tag && ch = None)
    satisfy predicate label
// /// parse a char 
let pstr str = 
    // label is just the character
    let label = sprintf "%s" str 

    let predicate (_,ch) = (ch = Some str) 
    satisfy predicate label 

let pany = 
    satisfy (fun _ -> true) "any"

open ParseLibrary
let debug it = 
    match it with 
    | Success(_, it) -> 
        it.position.node.pennPrint()
    | Failure (label,err,it) ->
        it.node.position.node.pennPrint()

let trees = fromTree "Roll a D6" |> Seq.head
trees.position.node.pennPrint()
let f = Option.bind moveNext
trees.position |> Some |> f |> Option.map (fun n -> n.node.pennPrint())

let panyUntiltag tag str =  manyUntil (ppen tag) pnstr |> mapP List.head >>. pstr str <?> (sprintf "%A : %s" tag str) 
let panyUntil str = manyUntil (pstr str) pnstr  |> mapP List.head <?> (sprintf ".* %s"  str) 
let roll = panyUntiltag NNP "Roll"
let a = panyUntil "a"
let d6 = panyUntil "D6"
let sentence = roll .>>. a .>>. d6 <?> "Roll a d6"
let toText = function _, Some text -> Str text |> Value |> Some | _ -> None

let parsedExpression = many pany

run sentence trees
debug it

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
//     |> mapP charListToStr 
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
//     |> mapP resultToInt
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
//     |> mapP resultToFloat
//     <?> label
