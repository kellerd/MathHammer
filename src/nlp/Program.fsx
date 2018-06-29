#load "../../.paket/load/netstandard2.0/System.IO.Compression.ZipFile.fsx"
#r "System.IO.Compression"
#r "System.Text.Encoding"
open System.IO
open System.IO.Compression
let modelsFolder = __SOURCE_DIRECTORY__ + @"\..\..\paket-files\nlp.stanford.edu\stanford-parser-full-2013-06-20\stanford-parser-3.2.0-models"
if Directory.Exists(modelsFolder) |> not then
    ZipFile.ExtractToDirectory(modelsFolder + ".jar", modelsFolder)

#r "../../../../../.nuget/packages/ikvm/8.1.5717/lib/IKVM.Runtime.dll"
#r "../../../../../.nuget/packages/ikvm/8.1.5717/lib/IKVM.OpenJDK.Core.dll"
#r "../../../../../.nuget/packages/ikvm/8.1.5717/lib/IKVM.OpenJDK.Text.dll"
#r "../../../../../.nuget/packages/stanford.nlp.parser/3.8.0/lib/ejml-0.23.dll"
#r "../../../../../.nuget/packages/stanford.nlp.parser/3.8.0/lib/slf4j-api.dll"
#r "../../../../../.nuget/packages/stanford.nlp.parser/3.8.0/lib/stanford-parser.dll"
#r "../../../../../.nuget/packages/stanford.nlp.parser.fsharp/0.0.14/lib/Stanford.NLP.Parser.Fsharp.dll"

open edu.stanford.nlp.parser.lexparser
open edu.stanford.nlp.trees
open java.util
open Stanford.NLP.FSharp.Parser
let model = modelsFolder + @"\edu\stanford\nlp\models\lexparser\englishPCFG.ser.gz"
let options = [|"-maxLength"; "500";
                "-retainTmpSubcategories";
                "-MAX_ITEMS"; "500000";
                "-outputFormat"; "penn,typedDependenciesCollapsed"|]
let parser = LexicalizedParser.loadModel(model, options)

let tlp = PennTreebankLanguagePack()
let gsf = tlp.grammaticalStructureFactory()

let getTree question =
    let tokenizer = tlp.getTokenizerFactory().getTokenizer(new java.io.StringReader(question))
    let sentence = tokenizer.tokenize()
    parser.apply(sentence)
let getKeyPhrases (tree:Tree) =
    tree.pennPrint()
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

let questions =
    [|"How to make an F# project work with the object browser"
      "How can I build WebSharper on Mono 3.0 on Mac?"
      "Adding extra methods as type extensions in F#"
      "How to get MonoDevelop to compile F# projects?"|]

questions
|> Seq.iter (fun question ->
    printfn "Question : %s" question
    question
    |> getTree
    |> getKeyPhrases
    |> List.rev
    |> List.iter (fun p ->
        p.getLeaves()
        |> Iterable.castToArray<Tree>
        |> Array.map(fun x-> x.label().value())
        |> printfn "\t%A")
)

let codexFolder = __SOURCE_DIRECTORY__ + @"\..\..\paket-files\codexes"
if Directory.Exists(codexFolder) |> not then
    let sourceFolder = @"C:\Users\diese\Downloads\"
    [ @"Warhammer 40,000 - Codex - Tyranids" ]
    |> List.iter (fun codex -> ZipFile.ExtractToDirectory(sourceFolder + codex + ".epub", codexFolder + @"\" + codex))

let filter8thCodex (file:string) = file.Contains("Army_Rules") || file.Contains("Army_List")
let codexes = 
    Directory.EnumerateDirectories(codexFolder)
    |> Seq.collect(fun folder -> //Path.GetFileName(folder), 
                             //folder, 
                             Directory.EnumerateFiles(folder, "*.xhtml", SearchOption.AllDirectories) 
                             |> Seq.filter filter8thCodex
                             |> Seq.map(function file -> Path.GetFileName(file),file)
                             )
    |> Seq.toList