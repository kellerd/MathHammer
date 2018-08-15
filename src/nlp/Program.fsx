#load @"../../.paket/load/netstandard2.0/System.IO.Compression.ZipFile.fsx"
open System
#r "netstandard"
#r "System.IO.Compression"
#r "System.Globalization"
#r "System.Text.Encoding"
#load @"../../.paket/load/netstandard2.0/FSharp.Data.fsx"
open FSharp.Data
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
    [|" When this unit manifests the Smite psychic power, it affects the closest visible enemy unit within 24\", instead of within 18\". In addition, it inflicts an additional D3 mortal wounds on that enemy unit if this unit contains 4 or 5 Zoanthropes, or
an additional 3 mortal wounds if it contains 6 Zoanthropes."
      "When manifesting or denying a psychic power with a Zoanthrope unit, first select a model in the unit – measure range, visibility etc. from this model. If this unit suffers Perils of the Warp, it suffers D3 mortal wounds as described in the core rules, but units within 6\" will only suffer damage if the Perils of the Warp causes the last model in the Zoanthrope unit to be slain."
      "You can re-roll failed charge rolls for units with this adaptation."
      "You can re-roll wound rolls of 1 in the Fight phase for units with this adaptation."
      "Use this Stratagem at the end of the Fight phase. Select a TYRANIDS unit from your army – that unit can immediately fight again."
      "If a rule requires you to roll a D3, roll a D6 and halve the result." |]
questions
|> Seq.skip 5
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
    let sourceFolder = @"%USERPROFILE%\Downloads\"
    [ @"Warhammer 40,000 - Codex - Tyranids" ]
    |> List.iter (fun codex -> ZipFile.ExtractToDirectory(sourceFolder + codex + ".epub", codexFolder + @"\" + codex))

let filter8thCodex (file:string) = file.Contains("Army_Rules") || file.Contains("Army_List")

// #load @"C:\Users\diese\Source\Repos\MathHammer\src\Check\Check.fs"
// #load @"C:\Users\diese\Source\Repos\MathHammer\src\Probability\Distribution.fs"
// #load @"C:\Users\diese\Source\Repos\MathHammer\src\GameActions\Primitives\Types.fs"
// #load @"C:\Users\diese\Source\Repos\MathHammer\src\Collections\Map.fs"
// #load @"C:\Users\diese\Source\Repos\MathHammer\src\Collections\List.fs"
// #load @"C:\Users\diese\Source\Repos\MathHammer\src\GameActions\Primitives\State.fs"
type Label = string 
type Characteristic = string 
type UnitName = string
type WeaponName = string
type WoundsRemaining = string
type Rule = 
    | LabelOnly of string 
    | RuleOnly of string 
    | LabelledRule of string * string
type Stratagem = Stratagem of cp:string * condition:string option * Rule    
type Weapon =  (Label * Characteristic) list   
type Category = string
type Weapons = Category option * Map<WeaponName, Weapon>
type Datasheet = 
    { 
        Powerlevel : int 
        UnitType : string
        Characteristics : Map<UnitName, (Label * Characteristic) list>
        Abilities : Rule list
        Weapons :  Map<WeaponName, Weapon>
        Keywords : string list 
        CharacteristicsDegrade : Map<WoundsRemaining,(Label * Characteristic) list> option
    } 
type Relic = string * Rule option * Weapon option    
type Unit = string * Datasheet
type Path = string    
type Folder = string   
type Page = 
    | Datasheet of Unit list
    | RuleDefs of Weapons : Weapons list * Rule list 
    | Stratagems of Stratagem list
    | Relics of Relic list
    | Points of (Category option * Map<(string), int>) list
    | Tactical of (int * Rule) list  
    | Psychic of (int * Rule) list  
    | Errors of Path
type Codex = { Folder: Folder; Pages : (Path*Page) seq }

type CodexParser = { 
    CodexFolder : Folder
    PageFilter : string -> bool 
    PageMap : Path -> Page
}
type TextBlock = {Top:float; Left:float; Bottom:float;Right:float}
let expandTextBlock { Top = t; Left = l; Bottom = b; Right = r } { Top    = top;Left   = left;Bottom = bottom; Right  = right }= 
    { Top    = max t top
      Left   = min l left
      Bottom = min b bottom
      Right  = max r right }
let (|LeftOf|_|) b a = 
    if a.Right < b.Left then Some (expandTextBlock a b)
    else None
let (|RightOf|_|) b a = 
    if a.Left > b.Right then Some (expandTextBlock a b)
    else None   
let (|Above|_|) b a = 
    if a.Bottom >= b.Top then Some (expandTextBlock a b)
    else None     
let (|Below|_|) b a = 
    if a.Top <= b.Bottom then Some (expandTextBlock a b)
    else None 
let (|OverAbove|_|) b a = 
    if a.Top <= b.Top then Some (expandTextBlock a b)
    else None 
let (|OverBelow|_|) b a = 
    if a.Top >= b.Top then Some (expandTextBlock a b)
    else None 
let (|SameRow|_|) b a =
    if a.Bottom <= b.Bottom && a.Top >= b.Bottom ||
       a.Top >= b.Top && a.Bottom <= b.Top ||
       a.Top <= b.Top && a.Bottom >= b.Bottom then Some (expandTextBlock a b)
    else None
let (|SameCell|_|) b a =
    if a.Left <= b.Left && a.Right >= b.Left ||
       a.Right >= b.Right && a.Left <= b.Right ||
       a.Right <= b.Right && a.Left >= b.Left then Some (expandTextBlock a b)
    else None
let (|SurroundsV|_|) b a = 
    if a.Bottom <= b.Bottom && a.Top >= b.Top then Some(expandTextBlock a b)
    else None
let (|SurroundsH|_|) b a = 
    if a.Left <= b.Left && a.Right >= b.Right then Some(expandTextBlock a b)
    else None
let (|Surrounds|_|) b a = 
    Option.map2 (fun a _ -> a) ((|SurroundsH|_|) b a)  ((|SurroundsV|_|) b a)
//divides a list L into chunks for which all elements match pred
let divide (func : _ -> bool) (sequence : _ seq) : _ list list =
    seq {
        use en = sequence.GetEnumerator ()
        let more = ref true
        while !more do
            let wasGood = ref true
            let sublist = 
                [
                    while !wasGood && en.MoveNext() do
                        if not (func en.Current) then yield en.Current
                        else wasGood := false
                ]
            if List.isEmpty sublist then more := false
            else yield sublist
    } |> Seq.toList

let divideInclusive (selector:'a->bool) source =
  let i = ref 0
  source
  |> List.groupBy (fun elem -> if selector elem then incr i
                               !i)
  |> List.map snd
open System.Text.RegularExpressions
let (|Matches|_|) pattern input =
    if input = null then None
    else
        let m = Regex.Match(input, pattern, RegexOptions.Compiled)
        if m.Success then Some [for x in m.Groups -> x]
        else None
let enumerateCodexes parser = 
    Directory.EnumerateDirectories(parser.CodexFolder)
    |> Seq.map(fun folder -> 
        { Folder = folder 
          Pages = Directory.EnumerateFiles(folder, "*.xhtml", SearchOption.AllDirectories) 
                  |> Seq.filter parser.PageFilter
                  |> Seq.map (fun path -> path, parser.PageMap path)
        } )
    |> Seq.toList

//let file = @"C:\Users\diese\Source\Repos\MathHammer\paket-files\codexes\Warhammer 40,000 - Codex - Tyranids\OEBPS\114-128_40K8_Tyranids_Army_Rules-13.xhtml"
//let file = @"C:\Users\diese\Source\Repos\MathHammer\paket-files\codexes\Warhammer 40,000 - Codex - Tyranids\OEBPS\114-128_40K8_Tyranids_Army_Rules-1.xhtml"
//let file = @"C:\Users\diese\Source\Repos\MathHammer\paket-files\codexes\Warhammer 40,000 - Codex - Tyranids\OEBPS\114-128_40K8_Tyranids_Army_Rules-4.xhtml"
let (|RuleDefinitions|) (file:string) =
    let body = HtmlDocument.Load(file).Body()
    body
let (|PointsValues|_|) (file:string) =
    let (RuleDefinitions body) = file
    body.CssSelect(".Background-Styles-40k8Codex_1--Headers-40k8Codex_1-3-Header-3-Sava---Sub-Section-40K8Codex")
    |> List.tryFind (fun node -> HtmlNode.innerText node = "POINTS VALUES")
    |> Option.map (fun _ -> body)
let (|Datasheets|_|) (file:string) =
    let (RuleDefinitions body) = file
    body.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-1-Datasheet-Header-Table-408Codex") 
    |> List.tryHead
    |> Option.map (fun _ -> body)
let (|Chapters|_|) (file:string) =
    let (RuleDefinitions body) = file
    body.CssSelect(".Background-Styles-40k8Codex_1--Headers-40k8Codex_1-4-Header-4-Sava-40K8Codex") 
    |> List.filter(fun n -> (not ((HtmlNode.innerText n).Contains("WARGEAR"))) )
    |> List.tryHead
    |> Option.map (fun _ -> body)
let (|WarlordTraits|_|) (file:string) =
    let (RuleDefinitions body) = file
    body.CssSelect(".Background-Styles-40k8Codex_1--Headers-40k8Codex_1-3-Header-3-Sava---Sub-Section-40K8Codex")
    @ body.CssSelect("._0K8---Rule-Styles_5--Warlord-Traits_5-1-Warlord-Trait-40K8Codex")
    |> List.tryFind (fun n -> n.InnerText().ToUpper().Contains("WARLORD TRAITS"))
    |> Option.map(fun _ -> body)
let (|Stratagems|_|) (file:string) =
    let (RuleDefinitions body) = file
    body.CssSelect("._0K8---Rule-Styles_4--Stratagems-40k8Codex_4-1-Stratagem-Name-40K8Codex")
    |> List.tryHead
    |> Option.map (fun _ -> body)
let (|Relics|_|) (file:string) =
    let (RuleDefinitions body) = file
    let titles =  body.CssSelect("._0K8---Rule-Styles_5--Warlord-Traits_5-1-Warlord-Trait-40K8Codex") |> List.tryHead 
    let weapons = body.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-5-Datasheet-Stat-Header-40K8")  |> List.tryHead
    Option.map2 (fun _ _ -> body) titles weapons
let (|TacticalObjectives|_|) (file:string) =
    let (RuleDefinitions body) = file
    body.CssSelect(".Background-Styles-40k8Codex_1--Headers-40k8Codex_1-3-Header-3-Sava---Sub-Section-40K8Codex")
    |> List.tryFind (fun n -> n.InnerText().ToUpper().Contains("TACTICAL OBJECTIVES"))
    |> Option.map(fun _ -> body)    
let (|Psychic|_|) (file:string) =
    let (RuleDefinitions body) = file
    let text = body.InnerText().ToUpper()
    if text.Contains("WARP CHARGE") && text.Contains("PSYKERS") then 
        Some body 
    else None
let emptyWeaponList = [None, Map.empty]    
let getText : HtmlNode list -> string = 
    Seq.collect(fun (n:HtmlNode) -> 
        n.Descendants["span"] 
        |> Seq.map(fun n -> n.InnerText().Trim()) 
        |> Seq.filter (String.IsNullOrEmpty >> not)
    ) >> String.concat " "
let getHeaderText xs  = 
        List.map(fun (n : HtmlNode) -> 
            n.CssSelect("span")
            |> Seq.map(fun n -> n.InnerText().Trim()) 
            |> Seq.filter (String.IsNullOrEmpty >> not)
            |> String.concat " " ) xs       
let getCategory (p,x) ys = 
    let p2 = List.minBy fst ys |> fst  
    if p < p2 then Some (getHeaderText x |> String.concat " ") else None   
let positionOf text span = 
    //let text = "top"
    //let span = node.CssSelect headerStyle |> Seq.head
    let iAmSpan = span |> HtmlNode.attributeValue "style" 
    let value = 
        if String.IsNullOrWhiteSpace(iAmSpan) |> not then iAmSpan
        else span.Descendants["span"] |> Seq.head |> HtmlNode.attributeValue "style" 
    let index = value.IndexOf(sprintf "%s:" text, StringComparison.InvariantCultureIgnoreCase) + (text.Length + 1)
    let index2 = value.IndexOf("px;", index, StringComparison.InvariantCultureIgnoreCase)
    value.Substring(index, index2 - index) |> float                
let getPosition n = 
    HtmlNode.descendantsNamed true ["span"] n 
    |> Seq.fold(fun blockPosition span ->
        //let span = n.Descendants["span"] |> Seq.head
        let top = positionOf "top" span
        let left = positionOf "left" span
        blockPosition 
        |> Option.map (expandTextBlock  {Top = top; Left = left; Right = left; Bottom = top})
        |> Option.defaultValue { Top    = top; Left   = left; Bottom = top; Right  = left }
        |> Some) None
let collectRows (currentPosition, nodes) (nextPosition,nextNode)  =
    match currentPosition, nextPosition with 
    | None, _ -> 
        match nodes with 
        | [] -> nextPosition, [nextNode] :: nodes
        | ns::otherRows ->  nextPosition, (nextNode::ns) :: otherRows
    | Some currentPosition, None -> 
        Some currentPosition, nodes
    | Some currentPosition, Some nextPosition -> 
        match nextPosition, nodes with 
        | _, [] -> 
            Some nextPosition, [nextNode] :: nodes
        | RightOf currentPosition _ & 
            SameRow currentPosition combinedPosition, ns::otherRows -> 
            Some combinedPosition,  (nextNode::ns) :: otherRows
        | RightOf currentPosition _ & 
            Above currentPosition combinedPosition, ns::otherRows -> //Futher text probably wouldn't be above unless it's a superscript
            Some combinedPosition,  (nextNode::ns) :: otherRows
        | RightOf currentPosition _ & 
            OverAbove currentPosition combinedPosition, ns::otherRows -> //Futher text probably wouldn't be above unless it's a superscript
            Some combinedPosition,  (nextNode::ns) :: otherRows
        | nextPosition, nodes -> 
            Some nextPosition, [nextNode] :: nodes
        
let getTable (node:HtmlNode) headerStyle tableStyle =   
    //let node = body.CssSelect(".Basic-Text-Frame > div").[4]
    let allHeaders = 
            node.CssSelect headerStyle 
            |> List.groupBy getPosition
            |> List.fold collectRows (None,[]) 
            |> snd
            |> List.map (fun ls -> ls |> List.collect id |> List.map (positionOf "top") |> List.max , ls |> List.rev |> List.collect id )
            |> List.sortBy (snd >> List.length) 
    let (category, headers) = 
        match allHeaders with 
        | [ x; ys] -> getCategory x [ys], ys |> snd |> getHeaderText
        | [ ys ]   -> None,               ys |> snd |> getHeaderText
        | []       -> None,               []
        | x :: ys -> getCategory x ys ,   List.map snd ys |> List.last |> getHeaderText
    let (table, rules) = 
        let table' = 
            node.CssSelect tableStyle
            |> List.groupBy getPosition //|> List.map (fun (p,t) -> p, getText t)
            |> List.fold collectRows (None,[]) 
            |> snd
        table'   
        |> List.mapi (fun i ns -> 
            let item = 
                if List.length ns = List.length headers - 1 then 
                    List.tryItem (i + 1) table' |> Option.bind (List.tryHead) |> Option.toList 
                else []             
            item @ ns |> List.rev)
        |> List.partition (fun l -> List.length l = List.length headers)
    let extraRules = List.collect id rules |> List.collect id              
    let tableValues = 
        table
        |> List.choose(fun tableRow -> 
            let labeledRows = 
                List.zip headers tableRow 
                |> List.map(fun (h,op) -> h, getText op)
                |> List.filter(fun (_,op) -> op <> "-")
            labeledRows 
            |> List.tryHead 
            |> Option.map (fun (_,name) -> (name, List.tail labeledRows) )
        ) |> Map.ofList 
    category, tableValues, extraRules
let parseDamageTable (table:HtmlNode) : Map<WoundsRemaining, (Label * Characteristic) list> = 
    let (_, dt, _) =
        getTable 
            table         
            "._0K8---Rule-Styles_3--Datasheet-Styles_3-7-Datasheet-Weapon-Stat-Header-Table-408Codex" 
            "._0K8---Rule-Styles_3--Datasheet-Styles_3-6-Datasheet-body-text-Table-408Codex"   
    dt
let applyDamageTable (datasheets,damageTable:HtmlNode list) (name,newDatasheet:Datasheet) = 
    match damageTable with 
    | [] -> (name,newDatasheet)::datasheets, damageTable
    | table::rest -> 
        if Map.tryPick (fun _ -> List.tryFind(fun (_,c) -> c = "*") ) newDatasheet.Characteristics |> Option.isSome then
            (name,{ newDatasheet with CharacteristicsDegrade = Some(parseDamageTable table)})::datasheets, rest
        else 
            (name,newDatasheet)::datasheets, damageTable
let matchUnitType str = 
    System.Text.RegularExpressions.Regex("image/[^_]+_([^_]+)_(Icon_)?Cutout.png").Replace(str, "$1")
let datasheets (body:HtmlNode) = 
    let extractStats (node:HtmlNode, (powerLevel, unitType)) = 
        //let node = sheet.[0]
        let keywords = 
            node.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-9-Datasheet-Keywords-Caps-408Codex")
            |> List.collect(fun kw -> kw.InnerText().Split(',') |> List.ofArray)
            |> List.map (fun (s:string) -> s.Trim())
        let (_, weapons, extraRules) = 
            getTable 
                node 
                "._0K8---Rule-Styles_3--Datasheet-Styles_3-7-Datasheet-Weapon-Stat-Header-Table-408Codex"
                "._0K8---Rule-Styles_3--Datasheet-Styles_3-7b-Datasheet-Weapon-Stat-Body-Table-408Codex"
        let (_, characteristics, extraRules') = 
            getTable 
                node 
                "._0K8---Rule-Styles_3--Datasheet-Styles_3-4-Datasheet-Stat-Header-Table-408Codex"
                "._0K8---Rule-Styles_3--Datasheet-Styles_3-5-Datasheet-Stat-body-Bold-Table-408Codex"                
        let rules = 
            node.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-6-Datasheet-body-text-Table-408Codex") 
            @ node.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-8-Datasheet-body-text-Table-Bullets-408Codex")
            @ extraRules
            @ extraRules'
            |> List.collect (fun rulesNodes -> 
                    //let rulesNodes = rules |> List.skip 1 |> List.head
                    let nodes = 
                        rulesNodes 
                        |> HtmlNode.descendantsNamed true ["span"] 
                        |> List.ofSeq
                    let index = 
                        nodes 
                        |> List.filter(fun n -> not ( (HtmlNode.innerText n).Trim() = "," || 
                                                      String.IsNullOrWhiteSpace(HtmlNode.innerText n)|| 
                                                       (HtmlNode.innerText n).Trim() = "(pg" ||
                                                       (HtmlNode.innerText n).Trim().EndsWith(")")
                                                    ))
                        |> List.tryFindIndex (fun n -> n.HasClass "CharOverride-18")
                    match nodes, index with 
                    | [], None -> [] // Empty and no text
                    | nodes, None -> 
                        nodes 
                        |> divide (fun n -> 
                            n.HasClass "CharOverride-18" && (HtmlNode.innerText n).Trim() = ",") 
                        |> List.map (fun l -> 
                            let text =
                                l 
                                |> List.map (fun n -> n.InnerText().Trim()) 
                                |> String.concat " " 
                            LabelOnly text)     // Some text, but no split, so all title
                    | nodes, Some 0 -> nodes |> List.map (fun n -> n.InnerText().Trim()) |> String.concat " " |> RuleOnly |> List.singleton  // Some text, but no split (found at 0), so all text               
                    | nodes, Some i -> 
                        let (title, text) = 
                            nodes 
                            |> List.map (fun n -> n.InnerText().Trim())
                            |> List.splitAt i 
                        LabelledRule(String.concat " " title, String.concat " " text) 
                        |> List.singleton
                    ) 

        let name = 
            node.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-1-Datasheet-Header-Table-408Codex > span")                        
            |> List.map (HtmlNode.innerText)
            |> String.concat ""
        name.Trim(), { Powerlevel = powerLevel
                       UnitType = unitType
                       Abilities = rules
                       Weapons = weapons
                       Keywords = keywords
                       Characteristics = characteristics
                       CharacteristicsDegrade = None} 
    let powerLevels = 
        body.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-3-Datasheet-Power-Rating-40K8Codex")  
        |> List.map (HtmlNode.innerText >> int)
    let unitTypes = 
        body.CssSelect("img[src*='_Cutout']") 
        |> List.map (HtmlNode.attributeValue "src" >> matchUnitType)  
    let unitStats = 
        let comboStats = List.zip powerLevels unitTypes
        let (sheet,damageTable) = 
            body.CssSelect(".Basic-Text-Frame") 
            |> List.filter(fun n -> n.CssSelect(".Background-Styles-40k8Codex_4--Captions-and-Lists-40K8Codex_4-7-Art-Caption-White-40K8Codex").IsEmpty && 
                                    n.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-3-Datasheet-Power-Rating-40K8Codex").IsEmpty) 
            |> List.splitAt (powerLevels.Length) 
        List.zip sheet comboStats
        |> List.map extractStats
        |> List.fold applyDamageTable ([],damageTable)
        |> fst
    unitStats |> Datasheet  
let chapters (body:HtmlNode) = 
    let rules = 
        body.CssSelect(".Basic-Text-Frame > div > p")
        |> List.filter(fun n -> (n.HasClass "Background-Styles-40k8Codex_2--Main-Text-40K8Codex_2-3-Body-Text-Semibold-Italic-40K8Codex" || 
                                 n.HasClass "Background-Styles-40k8Codex_1--Headers-40k8Codex_1-4-Header-4-Sava-40K8Codex") |> not)
        |> divideInclusive (fun n -> n.HasClass "Background-Styles-40k8Codex_1--Headers-40k8Codex_1-5-Header-5-Sava---Sub-header-40K8Codex")
        |> List.choose (function | [] -> None
                                 | [n] -> RuleOnly(getText [n]) |> Some
                                 | n::ns -> LabelledRule(getText [n], getText ns) |> Some )
    RuleDefs (emptyWeaponList, rules) 
let relics (body:HtmlNode) =
    body.CssSelect(".Basic-Text-Frame > div")
    |> List.collect(fun n -> n.Elements() )
    |> List.filter (fun n -> (n.HasClass "_0K8---Rule-Styles_5--Warlord-Traits_5-2-Warlord-Trait-Colour-Text-40K8Codex" ||
                              n.HasClass "Background-Styles-40k8Codex_2--Main-Text-40K8Codex_2-1-Body-Text-Minion-40K8Codex" ||
                              n.HasClass "Background-Styles-40k8Codex_2--Main-Text-40K8Codex_2-4-Intro-Minion-40K8Codex") |> not)
    |> divideInclusive(fun n -> n.HasClass "_0K8---Rule-Styles_5--Warlord-Traits_5-1-Warlord-Trait-40K8Codex")        
    |> List.collect (fun nodes -> 
        //let nodes = relics.[0]
        let (weapons,rules) = nodes |> List.partition (fun n -> HtmlNode.name n = "div")
        let n = HtmlNode.NewElement ("div",[],weapons) 
        let (_, weaponTable, extraRules) = 
            getTable 
                n
                "._0K8---Rule-Styles_3--Datasheet-Styles_3-5-Datasheet-Stat-Header-40K8"
                "._0K8---Rule-Styles_3--Datasheet-Styles_3-6-Datasheet-Stat-Body-40K8"  
        let rule =         
            match rules@extraRules with 
            | []  -> None
            | [n] -> RuleOnly(getText [n]) |> Some
            | n::ns -> LabelledRule(getText [n], getText ns) |> Some                     
        let ability = 
            n.CssSelect("._0K8---Rule-Styles_3--Datasheet-Styles_3-6-Datasheet-body-text-Table-408Codex") 
            |> List.collect(fun n -> n.Elements() |> List.skip 1)
        match ability with 
        | [] -> weaponTable 
        | ability -> 
            let rule =  [ HtmlNode.NewElement ("div", [], ability) ] |> getText 
            weaponTable 
            |> Map.map(fun _ item -> ("ABILITIES",rule)::item)
        |> Map.toList    
        |> List.map (function 
                     | name,[] -> name, rule, None
                     | name, weapons -> name, rule, Some weapons))
        |> Relics
let warlordTraits (body:HtmlNode) = 
    let rules = 
        body.CssSelect(".Basic-Text-Frame > div > p")
        |> List.filter(fun n -> (n.HasClass "_0K8---Rule-Styles_5--Warlord-Traits_5-1-Warlord-Trait-40K8Codex" || 
                                 n.HasClass "_0K8---Rule-Styles_5--Warlord-Traits_5-3-Warlord-Trait-Rules-Text-40K8Codex"))
        |> divideInclusive (fun n -> n.HasClass "_0K8---Rule-Styles_5--Warlord-Traits_5-1-Warlord-Trait-40K8Codex" )
        |> List.choose (function | [] -> None
                                 | [n] -> RuleOnly(getText [n]) |> Some
                                 | n::ns -> LabelledRule(getText [n], getText ns) |> Some )
        |> List.map (function 
                        | LabelledRule (Matches "[1-6] (.*)" [ _ ; label ],text) -> 
                            LabelledRule(label.Value, text) 
                        | r -> r )     
        |> List.filter (function RuleOnly "D6 RESULT" -> false | _ -> true)                            
    RuleDefs (emptyWeaponList, rules)
let stratagems (body:HtmlNode) = 
    let text = 
        body.CssSelect("p")
        |> List.filter (fun n -> n.HasClass "_0K8---Rule-Styles_4--Stratagems-40k8Codex_4-1-Stratagem-Name-40K8Codex"  ||
                                 n.HasClass "_0K8---Rule-Styles_4--Stratagems-40k8Codex_4-2-Strategem-Type-Style-40K8Codex" ||
                                 n.HasClass "_0K8---Rule-Styles_4--Stratagems-40k8Codex_4-3-Strategem-Body-Text-40K8Codex")
        |> List.chunkBySize 3  
    let costs = 
        body.CssSelect("._0K8---Rule-Styles_4--Stratagems-40k8Codex_4-4-Stratagem-Cost-40K8Codex") 
        |> List.map (HtmlNode.innerText)    
    List.zip text costs        
    |> List.map(function 
        | [ label; condition; text ], cost -> 
            Stratagem(cost, getText [condition] |> Some, LabelledRule(getText [label], getText [text]))
        | [ label; text ], cost -> 
            Stratagem(cost, None, LabelledRule(getText [label], getText [text]))
        | rule, cost ->
            Stratagem(cost, None, RuleOnly(getText rule))
    ) 
    |> Stratagems
let rules (body:HtmlNode) = 
    let (weapons,rules)  =
        body.CssSelect(".Basic-Text-Frame > div")
        |> List.map(fun n -> 
            //let (weapons,rules)  =
                n.Elements()  
                |> List.filter (fun n -> (n.HasClass "Background-Styles-40k8Codex_2--Main-Text-40K8Codex_2-4-Intro-Minion-40K8Codex" || 
                                          n.HasClass "Background-Styles-40k8Codex_2--Main-Text-40K8Codex_2-3-Body-Text-Semibold-Italic-40K8Codex" || 
                                          n.HasClass "Background-Styles-40k8Codex_3--Quotes-and-Other-40K8Codex_3-1-Quote-Direct-Speech-40K8Codex" || 
                                          n.HasClass "Background-Styles-40k8Codex_1--Headers-40k8Codex_1-3-Header-3-Sava---Sub-Section-40K8Codex" || 
                                          n.HasClass "Background-Styles-40k8Codex_2--Main-Text-40K8Codex_2-2-Body-Text-Bold-40K8Codex" || 
                                          n.HasClass "Background-Styles-40k8Codex_3--Quotes-and-Other-40K8Codex_3-3-Quote-Attribution-40K8Codex" ||
                                          n.HasClass "Background-Styles-40k8Codex_1--Headers-40k8Codex_1-2-Header-2-Sava---Section-Header-40K8Codex") |> not)
                |> divideInclusive(fun n -> n.HasClass "Background-Styles-40k8Codex_1--Headers-40k8Codex_1-5-Header-5-Sava---Sub-header-40K8Codex" ||
                                            n.HasClass "Background-Styles-40k8Codex_1--Headers-40k8Codex_1-6-Header-6-Sava-40K8Codex")        
                |> List.map (fun nodes -> 
                //let nodes = rulesText.[0]
                    let (weapons,rules) = nodes |> List.partition (fun n -> HtmlNode.name n = "div")
                    let n = HtmlNode.NewElement ("div",[], weapons)
                    let (category, weaponTable, extraRules) = 
                        getTable 
                            n
                            "._0K8---Rule-Styles_3--Datasheet-Styles_3-7-Datasheet-Weapon-Stat-Header-Table-408Codex"
                            "._0K8---Rule-Styles_3--Datasheet-Styles_3-7b-Datasheet-Weapon-Stat-Body-Table-408Codex" 
                    let rule =         
                        match rules@extraRules with 
                        | []  -> None
                        | [n] -> RuleOnly(getText [n]) |> Some
                        | n::ns -> LabelledRule(getText [n], getText ns) |> Some   
                    (category, weaponTable),rule )   
                |> List.unzip    
            //weapons, rules            
        ) |> List.unzip     
        //weapons |> List.collect id |> List.head |> snd |> Map.iter (printfn "%s %A")     
    RuleDefs(List.collect id weapons |> List.filter(fun (_,m) -> Map.count m > 0), rules |> List.collect id |> List.collect (Option.toList) )       
let tactical (body:HtmlNode) = 
    let cardData = 
        body.CssSelect("p")
        |> List.filter (fun n -> n.HasClass "_0K8---Rule-Styles_2--Tactical-Objectives-408Codex_2-4-Tactical-Objective-Rules-Text-40K8Codex" ||
                                 n.HasClass "_0K8---Rule-Styles_3--Datasheet-Styles_3-6-Datasheet-Stat-Body-40K8" ) 
        |> List.map (List.singleton)
    let (labels,conditions) = 
        cardData        
        |> List.splitAt (cardData.Length * 2 / 3)
    let (points,names) = 
        labels 
        |> List.mapi(fun i l -> i,l) 
        |> List.partition(fun (i,_) -> i % 2 = 0)
    List.zip (List.map (snd >> getText) names) (List.map getText conditions)
    |> List.map LabelledRule
    |> List.zip (List.map (snd >> getText >> int) points) 
    |> Tactical
let psychic (body:HtmlNode) = 
    let cardData = 
        body.CssSelect("p")
        |> List.filter (fun n -> (n.HasClass "_0K8---Rule-Styles_5--Warlord-Traits_5-1-Warlord-Trait-40K8Codex" ||
                                  n.HasClass "_0K8---Rule-Styles_5--Warlord-Traits_5-3-Warlord-Trait-Rules-Text-40K8Codex") && 
                                  not (getText [n] = "D6 RESULT") ) 
        |> List.map (List.singleton)
    let (labels,conditions) = 
        cardData   
        |> List.mapi(fun i l -> (i,l))
        |> List.partition(fun (i,_) -> i % 2 = 0)
    let (points,names) = 
        labels 
        |> List.map(snd >> getText >> (fun s -> let [|p;n|] = s.Split([|' '|], 2) in p,n)) 
        |> List.unzip

    List.zip (names) (List.map (snd >> getText) conditions)
    |> List.map LabelledRule
    |> List.zip (List.map (int) points) 
    |> Psychic
let pointsValues (body:HtmlNode) = 
    body.CssSelect(".Basic-Text-Frame > div")
    |> List.map(fun node -> 
        let (category,weapons,_)  =
            getTable 
                node
                "p[class*=Header-40K8]"
                "._0K8---Rule-Styles_3--Datasheet-Styles_3-7b-Datasheet-Weapon-Stat-Body-Table-408Codex"                      
        category, weapons 
                   |> Map.map (fun _ -> List.tryLast >> Option.map (fun (_, pt) -> int pt))
                   |> Map.filter (fun _ -> function Some v -> true | _ -> false)
                   |> Map.map (fun _ -> Option.get)
    ) 
    |> List.filter(fun (_,m) -> Map.count m > 0)
    |> Points  
let map8thCodex (file:Path) = 
    match file with 
    | Datasheets body          -> datasheets body
    | Chapters body            -> chapters body 
    | WarlordTraits body       -> warlordTraits body
    | Stratagems body          -> stratagems body
    | PointsValues body        -> pointsValues body
    | Relics body              -> relics body
    | TacticalObjectives body  -> tactical body
    | Psychic body             -> psychic body
    | RuleDefinitions body     -> rules body 

let file = Path.Combine(codexFolder, @"Warhammer 40,000 - Codex - Tyranids\OEBPS\082-097_40K8_Tyranids_Army_List_01-13.xhtml")
let (RuleDefinitions body) = file
let proc = System.Diagnostics.Process.Start("iexplore", file)
// rules body |> printfn " %A"
// let sheet = body.CssSelect(".Basic-Text-Frame")
// sheet.Length
// let node = sheet.[0]
//let node = sheet.[1]
//let node = sheet.[2]

// let headerStyle = "._0K8---Rule-Styles_3--Datasheet-Styles_3-7-Datasheet-Weapon-Stat-Header-Table-408Codex"
// let tableStyle  = "._0K8---Rule-Styles_3--Datasheet-Styles_3-7b-Datasheet-Weapon-Stat-Body-Table-408Codex"   
// let headerStyle = "._0K8---Rule-Styles_3--Datasheet-Styles_3-4-Datasheet-Stat-Header-Table-408Codex"
// let tableStyle  = "._0K8---Rule-Styles_3--Datasheet-Styles_3-5-Datasheet-Stat-body-Bold-Table-408Codex"
//@"c:\Users\diese\Source\Repos\MathHammer\src\nlp\..\..\paket-files\codexes\Warhammer 40,000 - Codex - Tyranids\OEBPS\098-113_40K8_Tyranids_Army_List_02-13.xhtml" |> map8thCodex
let failGracefully f file  = 
    try 
        f file
    with ex -> 
        printfn "Failed parsing file %s" file
        printfn "Error: %A" ex.Message
        Errors file
let EigthEdition = {
    PageFilter = filter8thCodex
    CodexFolder = codexFolder
    PageMap = failGracefully map8thCodex
}
let codexes = enumerateCodexes EigthEdition
let nidCodex = codexes |> List.head 
let pages = 
    nidCodex.Pages 
    // |> Seq.filter (function 
    //     | (Stratagems _, _) -> false
    //     | (Chapters _, _) -> false
    //     | (PointsValues _, _) -> false
    //     | (Relics _, _) -> false
    //     | (WarlordTraits _, _) -> false
    //     | (Datasheets _, _) -> false
    //     |(RuleDefinitions _, _) -> true ) 
    |> Seq.toList
    // |> Seq.iter (printf "%A")
let sheets = pages|> List.filter(snd >> function Datasheet _ -> true | _ -> false)
let errors = pages |> List.map (snd) |> List.filter(function Errors _ -> true | _ -> false)
nidCodex.Pages 
|> Seq.filter (function 
        | (Stratagems _, _) -> false
        | (Chapters _, _) -> false
        | (PointsValues _, _) -> false
        | (Relics _, _) -> false
        | (WarlordTraits _, _) -> false
        | (Datasheets _, _) -> false
        |(RuleDefinitions _, _) -> true ) 
|> Seq.iter(fst >> printfn "%s")
let sheetsFilenames = 
    nidCodex.Pages 
    |> Seq.filter (function 
        | (Stratagems _, _) -> false
        | (Chapters _, _) -> false
        | (PointsValues _, _) -> false
        | (Relics _, _) -> false
        | (WarlordTraits _, _) -> false
        | (Datasheets _, _) -> false
        |(RuleDefinitions _, _) -> true ) 
    |> Seq.map(fun (file,_) -> (|RuleDefinitions|) file)
    |> Seq.toList

// let body = stratagems sheets.[0]    