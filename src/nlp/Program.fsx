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

#load @"C:\Users\diese\Source\Repos\MathHammer\src\Check\Check.fs"
#load @"C:\Users\diese\Source\Repos\MathHammer\src\Probability\Distribution.fs"
#load @"C:\Users\diese\Source\Repos\MathHammer\src\GameActions\Primitives\Types.fs"
#load @"C:\Users\diese\Source\Repos\MathHammer\src\Collections\Map.fs"
#load @"C:\Users\diese\Source\Repos\MathHammer\src\Collections\List.fs"
#load @"C:\Users\diese\Source\Repos\MathHammer\src\GameActions\Primitives\State.fs"
open GameActions.Primitives.Types
open GameActions.Primitives.State
type Datasheet = 
    { Powerlevel : GameActions.Primitives.Types.Operation 
      UnitSize : Operation 
      Characteristics : Map<string, Operation>
      Abilities : Operation list
      Weapons : Map<string, Operation>
      Keywords : Operation list 
      Factions : Operation list   } 
type Unit = string * Datasheet
type Path = string    
type Folder = string   
type Page = 
    | Datasheet of Unit list
    | RuleDefs of Operation list 
    | Points of Map<(string*string), Operation>
type Codex = { Folder: Folder; Pages : Page list }

type CodexParser = { 
    CodexFolder : Folder
    PageFilter : string -> bool 
    PageMap : Path -> Page
}
#r @"../../../../../.nuget\packages/FSharp.Data/2.4.6/lib/net45/FSharp.Data.dll"
open FSharp.Data
let enumerateCodexes parser = 
    Directory.EnumerateDirectories(parser.CodexFolder)
    |> Seq.map(fun folder -> 
        { Folder = folder 
          Pages = Directory.EnumerateFiles(folder, "*.xhtml", SearchOption.AllDirectories) 
                  |> Seq.filter parser.PageFilter
                  |> Seq.map parser.PageMap
                  |> Seq.toList 
        } )


//let file = @"C:\Users\diese\Source\Repos\MathHammer\paket-files\codexes\Warhammer 40,000 - Codex - Tyranids\OEBPS\114-128_40K8_Tyranids_Army_Rules-13.xhtml"
//let file = @"C:\Users\diese\Source\Repos\MathHammer\paket-files\codexes\Warhammer 40,000 - Codex - Tyranids\OEBPS\114-128_40K8_Tyranids_Army_Rules-1.xhtml"
//let file = @"C:\Users\diese\Source\Repos\MathHammer\paket-files\codexes\Warhammer 40,000 - Codex - Tyranids\OEBPS\114-128_40K8_Tyranids_Army_Rules-4.xhtml"
open FSharp.Data
let (|RuleDefinitions|) (file:string) =
    let body = HtmlDocument.Load(file).Body()
    body
let (|PointsValues|_|) (file:string) =
    let (RuleDefinitions body) = file
    body.CssSelect(".Background-Styles-40k8Codex_1--Headers-40k8Codex_1-3-Header-3-Sava---Sub-Section-40K8Codex")
    |> List.tryFind (fun node -> node.InnerText() = "POINTS VALUES")
    |> Option.map (fun _ -> body)
let (|Datasheets|_|) (file:string) =
    let (RuleDefinitions body) = file
    body.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-1-Datasheet-Header-Table-408Codex") 
    |> List.tryHead
    |> Option.map (fun _ -> body)
let (|Chapters|_|) (file:string) =
    let (RuleDefinitions body) = file
    body.CssSelect(".Background-Styles-40k8Codex_1--Headers-40k8Codex_1-4-Header-4-Sava-40K8Codex") 
    |> List.tryHead
    |> Option.map (fun _ -> body)
let (|Stratagems|_|) (file:string) =
    let (RuleDefinitions body) = file
    body.CssSelect(".Background-Styles-40k8Codex_1--Headers-40k8Codex_1-3-Header-3-Sava---Sub-Section-40K8Codex")
    |> List.tryFind (fun node -> node.InnerText() = "STRATAGEMS")
    |> Option.map (fun _ -> body)
let (|Relics|_|) (file:string) =
    let (RuleDefinitions body) = file
    let titles =  body.CssSelect("._0K8---Rule-Styles_5--Warlord-Traits_5-1-Warlord-Trait-40K8Codex") |> List.tryHead 
    let weapons = body.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-5-Datasheet-Stat-Header-40K8")  |> List.tryHead
    Option.map2 (fun _ _ -> body) titles weapons
let (|WarlordTraits|_|) (file:string) =
    let (RuleDefinitions body) = file
    body.CssSelect("._0K8---Rule-Styles_5--Warlord-Traits_5-1-Warlord-Trait-40K8Codex")  
    |> List.tryHead
    |> Option.map (fun _  -> body) 

// match file with 
// | PointsValues f -> f
// | Datasheets f -> f
// | Chapters f -> f
// | RuleDefinitions f -> f

let map8thCodex (file:Path) = 
    let datasheets (body:HtmlNode) = 
        let file = @"C:\Users\diese\Source\Repos\MathHammer\paket-files\codexes\Warhammer 40,000 - Codex - Tyranids\OEBPS\082-097_40K8_Tyranids_Army_List_01-9.xhtml"
        let (RuleDefinitions body) = file
        let powerLevels = 
            body.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-3-Datasheet-Power-Rating-40K8Codex")  
            |> List.map (fun node -> Value (Str (node.InnerText())))
        let getImgSrc (node :HtmlNode) = (node.Attribute "src").Value()
        let matchUnitType (str:string) = 
            let ut = (new System.Text.RegularExpressions.Regex("image/[^_]+_([^_]+)_Icon_Cutout.png")).Replace(str, "$1")
            Value (Str ut)
        let unitTypes = 
            body.CssSelect("img[src*='Icon_Cutout']") 
            |> List.map (getImgSrc >> matchUnitType)

        let extractStats (node:HtmlNode, (powerLevel, unitType)) = 
            //let node = sheet |> List.head
            //let powerLevel = powerLevels |> List.head
            //let unitType = unitTypes |> List.head
            let keywords = 
                node.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-9-Datasheet-Keywords-Caps-408Codex")
                |> List.collect(fun kw -> kw.InnerText().Split(',') |> List.ofArray)
                |> List.map(fun s -> 
                    if s.Trim().StartsWith("<") then labelVar "Chapter"
                    else Str (s.Trim()) |> Value)
            let parseRules title (s:string) = 
                if (s.StartsWith("(pg")) && Option.isSome title then Var (Option.get title)
                else s.Trim() |> Str |> Value
            let rules = 
                node.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-6-Datasheet-body-text-Table-408Codex") 
                |> List.choose (
                        HtmlNode.descendantsNamed true ["span"] 
                        >> Seq.fold (function 
                                     | a, Some text -> fun n -> a, Some ((n.InnerText()) :: text)
                                     | Some title, None -> fun n -> if n.HasClass "CharOverride-17" then Some (n.InnerText() :: title), None
                                                                    else Some title, Some [n.InnerText()]
                                     | None, None -> fun n -> if n.HasClass "CharOverride-17" then Some [n.InnerText()], None
                                                              else None, Some [n.InnerText()]  ) (None,None)
                        >> function 
                            | None, None -> None 
                            | Some title, Some text -> 
                                let title' = (List.rev title |> String.concat "").Trim() 
                                let text' = (List.rev text |> String.concat "") |> parseRules (Some title')
                                pair (Value(Str(title'))) text' |> Some
                            | Some title, None -> (List.rev title |> String.concat "").Trim()  |> Str |> Value |> Some
                            | None, Some text -> (List.rev text |> String.concat "") |> parseRules None |> Some
                        ) 
                                    
            let params = 
                Value(ParamArray [ Value (Str "Power Level"); powerLevel])
                :: Value(ParamArray [ Value (Str "Unit Types"); unitType])
                :: keywords @ rules
                |> ParamArray
                |> Value
            params
        let unitStats = 
            let comboStats = List.zip powerLevels unitTypes
            let sheet = body.CssSelect(".Basic-Text-Frame") |> List.truncate (powerLevels.Length) 
            List.zip sheet comboStats
            |> List.map extractStats

        []|> Datasheet  
    let chapters (body:HtmlNode) = NoValue |>Value |> List.singleton |> RuleDefs
    let relics (body:HtmlNode) = NoValue |>Value |> List.singleton |> RuleDefs
    let warlordTraits (body:HtmlNode) = NoValue |>Value |> List.singleton |> RuleDefs
    let stratagems (body:HtmlNode) = NoValue |>Value |> List.singleton |> RuleDefs
    let rules (body:HtmlNode) = NoValue |>Value |> List.singleton |> RuleDefs
    let pointsValues (body:HtmlNode) = Map.empty<_,_>|> Points  

    match file with 
    | PointsValues body        -> pointsValues body
    | Datasheets body          -> datasheets body
    | Chapters body            -> chapters body
    | Stratagems body          -> stratagems body
    | Relics body              -> relics body
    | WarlordTraits body       -> warlordTraits body
    | RuleDefinitions body     -> rules body 

let EigthEdition = {
    PageFilter = filter8thCodex
    CodexFolder = codexFolder
    PageMap = map8thCodex
}
let codexes = enumerateCodexes EigthEdition
codexes |> Seq.toList