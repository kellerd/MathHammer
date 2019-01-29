#load "ParseCodexes.fsx"
open ParseCodexes
open System.IO
open System.IO.Compression
#load "NLP.fsx"
open NLP
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

//let gsf = tlp.grammaticalStructureFactory()
// #load @"..\GameActions\Primitives\Types.fs"
// #load @"..\GameActions\Primitives\State.fs"
open ParseCodexes
// open GameActions.Primitives.Types
// open GameActions.Primitives.State    
let parseRule = 
    function 
    | LabelledRule(n,s) -> Some n, (nlpRule s)
    | RuleOnly s -> None, nlpRule s
    | LabelOnly n -> Some n, nlpRule ""
let vStr s = s |> escape_string |> Str |> Value
let opList ops = ParamArray(ops) |> Value
let pair x y = opList [ x; y ]
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
        let rules = List.map parseRule rl 
        let weapons = List.collect (snd >> Map.toList >> List.map (fun (name,weapon) -> Some name, parseWeapon weapon) ) wl
        page, 
            (rules @ weapons)
            |> List.filter (fun (name,_) -> name <> Some "ABILITIES")
            |> List.distinctBy fst
    | Page.Stratagems g -> 
        page, 
        List.map (fun (Stratagem (cp,condition,rule)) -> 
            let name, ruleText = parseRule rule
            let cpCheck = 
                let name' = Option.defaultValue "" name
                let cpChoice cpList = 
                    let cpChoices = List.map (fun cp -> string cp, IfThenElse(App(Call GreaterThan, opList [Var "Available CP"; Var name']), ruleText, None)) cpList
                    ("<Not selected>", ruleText) :: cpChoices

                match cp with
                | CpChoice(cp1, cp2) -> Choice(name', cpChoice [cp1; cp2])
                | Cp (cp)            -> Choice(name', cpChoice [cp])
            let rule =               
                match condition with 
                | Some condition -> IfThenElse(App(Call Contains, opList [Var "Keywords"; vStr condition]), cpCheck, None)   
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
        let errDebug = 
            sprintf """
open FSharp.Data
let file = @"%s"
let (RuleDefinitions body) = file
let sheet = body.CssSelect(".Basic-Text-Frame")
let node = sheet.[0]
let headerStyle = "%s"
let tableStyle = "%s"
%s body

            """ path
        let debugMessage = 
            match path with 
            | Datasheets (body)          ->  
                errDebug 
                    "._0K8---Rule-Styles_3--Datasheet-Styles_3-7-Datasheet-Weapon-Stat-Header-Table-408Codex"
                    "._0K8---Rule-Styles_3--Datasheet-Styles_3-7b-Datasheet-Weapon-Stat-Body-Table-408Codex" "datasheets" 
                     +
                errDebug   
                    "._0K8---Rule-Styles_3--Datasheet-Styles_3-4-Datasheet-Stat-Header-Table-408Codex"
                    "._0K8---Rule-Styles_3--Datasheet-Styles_3-5-Datasheet-Stat-body-Bold-Table-408Codex"
                    "datasheets"               
            | Chapters (body)            ->  
                errDebug "" ""  "chapters"         
            | WarlordTraits (body)       ->  
                errDebug  "" "" "warlordTraits"
            | IsStratagem (body)          ->  
                errDebug  "" "" "stratagems"
            | PointsValues (body)        ->  
                errDebug 
                    "._0K8---Rule-Styles_3--Datasheet-Styles_3-7-Datasheet-Weapon-Stat-Header-Table-408Codex"
                    "._0K8---Rule-Styles_3--Datasheet-Styles_3-7b-Datasheet-Weapon-Stat-Body-Table-408Codex"
                    "pointsValues"
            | Relics (body)              ->  
                errDebug 
                    "._0K8---Rule-Styles_3--Datasheet-Styles_3-5-Datasheet-Stat-Header-40K8"
                    "._0K8---Rule-Styles_3--Datasheet-Styles_3-6-Datasheet-Stat-Body-40K8"  
                    "relics"
            | TacticalObjectives (body)  ->  
                errDebug "tactical" "" ""
            | Psychic (body)             ->  
                errDebug "psychic" "" ""
            | RuleDefinitions (body)     ->  
                errDebug 
                    "._0K8---Rule-Styles_3--Datasheet-Styles_3-7-Datasheet-Weapon-Stat-Header-Table-408Codex"
                    "._0K8---Rule-Styles_3--Datasheet-Styles_3-7b-Datasheet-Weapon-Stat-Body-Table-408Codex" 
                    "rules"
        printfn "%s" (sprintf "%s" debugMessage)                    
        page, [ None, g |> escape_string |> sprintf "Error parsing file: %s" |> Str |> Value ]

let codexPath codexName =  
    Path.Combine(__SOURCE_DIRECTORY__, sprintf "Output\\%s" codexName)
let filename codexName file = 
    let output = codexPath codexName
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
let startOver (codexName:string) file =
    let outfile = 
        match file with 
        | Datasheet _ -> "Datasheet"
        | RuleDefs _ -> "Rule"
        | Page.Stratagems _ -> "Stratagem"
        | Page.Relics _ -> "Relic"
        | Points _ -> "Point"
        | Tactical _ -> "Tactical"
        | Page.Psychic _ -> "Psychic"
        | Errors _ -> "Error" 
    let init = 
        sprintf """namespace ``%s`` 
module %s = 
    #if INTERACTIVE
    #load @"..\..\..\Check\Check.fs"
    #load @"..\..\..\Probability\Distribution.fs"
    #load @"..\..\..\GameActions\Primitives\Types.fs"
    #load @"..\..\..\GameActions\Primitives\GamePrimitiveOperations.fs"
    #load @"..\..\..\GameActions\Primitives\TypeChecker.fs"
    #load @"..\..\..\Collections\Map.fs"
    #load @"..\..\..\Collections\List.fs"
    #load @"..\..\..\GameActions\Primitives\State.fs"
    #endif
    open GameActions.Primitives.Types
    open GameActions.Primitives.State  
    let _ = ignore
"""
    
    let outfilePath = filename codexName file
    File.Delete(outfilePath)
    File.AppendAllText(outfilePath, init codexName outfile)    
let outputToDirectory codexName (file, rules)= 
    let outfilePath = filename codexName file
    List.iter (fun (name, rule) -> 
        let text = 
            match name with 
            | Some name -> (sprintf "let ``%s`` = \r\n%A" name rule)
            | None -> (sprintf "let _ = \r\n%A"  rule)
        File.AppendAllText(outfilePath, sprintf "    %s\r\n" (text.Replace("\n", "\n        ")))
    ) rules
    
let exportCodexes codex = 
    let codexName = Path.GetFileName(codex.Folder)
    codex.Pages
    |> Array.ofSeq
    |> Array.Parallel.map parseRules
    |> Seq.iter (outputToDirectory codexName)

open System.IO
let cleanDir codex = 
    Directory.EnumerateFiles(codex.Folder) 
    |> Seq.iter(File.Delete)

    let codexName = Path.GetFileName(codex.Folder) 

    codex.Pages
    |> Seq.toList 
    |> List.iter (snd >> startOver codexName)
let codex = nidCodex
cleanDir codex
exportCodexes codex