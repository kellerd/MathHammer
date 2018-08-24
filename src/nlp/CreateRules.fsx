#load "ParseCodexes.fsx"
open System.IO
open System.IO.Compression
#load @"..\Check\Check.fs"
#load @"..\Probability\Distribution.fs"
#load @"..\GameActions\Primitives\Types.fs"
#load @"..\Collections\Map.fs"
#load @"..\Collections\List.fs"
#load @"..\GameActions\Primitives\State.fs"

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
open ParseCodexes
open GameActions.Primitives.Types
open GameActions.Primitives.State    

let nlpRule (s:string) = s.Replace("\"", "\\\"") |> Str |> Value
let parseRule = 
    function 
    | LabelledRule(n,s) -> Some n, (nlpRule s)
    | RuleOnly s -> None, nlpRule s
    | LabelOnly n -> Some n, nlpRule ""
let parseWeapon (weapon:Weapon) = 
    List.map (fun (label,characteristic) -> 
        if label = "ABILITIES" then 
            pair (vStr label) (nlpRule characteristic)
        else pair (vStr label) (vStr characteristic)      
    ) weapon
    |> ParamArray 
    |> Value  
let parseRules (path,page) = 
    match page with 
    | Datasheet g ->         
        page, List.map(fun (name,datasheet) -> 
            Some name, Value(NoValue)
        ) g 
    | RuleDefs (wl,rl) -> 
        page, 
            List.map parseRule rl @ 
            List.collect (snd >> Map.toList >> List.map (fun (name,weapon) -> Some name, parseWeapon weapon) ) wl
    | Page.Stratagems g -> 
        page, 
        List.map (fun (Stratagem (cp,condition,rule)) -> 
            let name, ruleText = parseRule rule
            let rule =               
                let cpCheck = IfThenElse(App(Call GreaterThan, opList [get "Available CP"; vInt cp]), ruleText, None)  
                match condition with 
                | Some condition -> IfThenElse(App(Call Contains, opList [get "Keywords"; vStr condition]), cpCheck, None)   
                | None -> cpCheck
            name, rule ) g
    | Page.Relics g -> 
        page, List.map(fun (name, rule, relic) -> 
            let name' = Some name
            let rule' = Option.map parseRule rule
            let relic' = Option.map parseWeapon relic
            match rule', relic' with 
            | Some (_, rule), Some relic -> name', (ParamArray [rule; relic] |> Value)
            | Some (_, rule), None       -> name', rule
            | None, Some relic           -> name', relic 
            | None, None                 -> name', (NoValue |> Value) ) g
    | Points g -> page, [None, Value(NoValue)]//tokenizePoints g
    | Tactical g -> 
        page, List.map(snd >> parseRule) g
    | Page.Psychic g -> 
        page, List.map(snd >> parseRule) g
    | Errors g -> 
        page, [ None, (sprintf "Error parsing file: %s" g |> Str |> Value) ]
let filename codexName file = 
    let output = Path.Combine(__SOURCE_DIRECTORY__, sprintf "Output\\%s" codexName)
    Directory.CreateDirectory(output) |> ignore
    let outfile = 
        match file with 
        | Datasheet _ -> "Datasheet.fs"
        | RuleDefs _ -> "RuleDefs.fs"
        | Page.Stratagems _ -> "Stratagems.fs"
        | Page.Relics _ -> "Relics.fs"
        | Points _ -> "Points.fs"
        | Tactical _ -> "Tactical.fs"
        | Page.Psychic _ -> "Psychic.fs"
        | Errors _ -> "Errors.fs"
    Path.Combine(output,outfile)     
let startOver codexName file = 
    let init = 
        """#if INTERACTIVE
#load @"..\..\..\Check\Check.fs"
#load @"..\..\..\Probability\Distribution.fs"
#load @"..\..\..\GameActions\Primitives\Types.fs"
#load @"..\..\..\Collections\Map.fs"
#load @"..\..\..\Collections\List.fs"
#load @"..\..\..\GameActions\Primitives\State.fs"
#endif
"""
    let outfilePath = filename codexName file
    File.Delete(outfilePath)
    File.AppendAllText(outfilePath, init)    
let outputToDirectory codexName (file, rules)= 
    let outfilePath = filename codexName file
    File.AppendAllText(outfilePath,sprintf "%A" rules)
    
let exportCodexes codex = 
    let codexName = Path.GetFileName(codex.Folder)
    codex.Pages
    |> Seq.map parseRules
    |> Seq.iter (outputToDirectory codexName)

let cleanDir codex = 
    let codexName = Path.GetFileName(codex.Folder)
    codex.Pages
    |> Seq.iter (snd >> startOver codexName)

nidCodex |> cleanDir
nidCodex |> exportCodexes

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
