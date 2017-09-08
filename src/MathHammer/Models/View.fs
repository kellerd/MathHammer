module MathHammer.Models.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Fable.Import
open GameActions.Primitives.Types
open Result
open Distribution
open Probability.View
let onClick x : IProp = OnClick(x) :> _

let showActions dispatch operation  = 
      GameActions.Primitives.View.root operation dispatch

let showAttributes (key, operation) dispatch = 
      div [ClassName "has-text-centered column"]
          [ b  [] [str key]
            br []
            div [] (GameActions.Primitives.View.root operation dispatch) ]
let rangeStops (dist:Distribution<_>)  = 
    let length = List.length dist
    let minRange, maxRange,minProbability,maxProbability =
        dist 
        |> Distribution.choose (function IntResult (i) -> Result.map ((*) 1<inch>) i |> Some | _ -> None)
        |> List.fold (fun (currMinRange,currMaxRange,currMin,currMax) (range,prob) -> 
            min currMinRange range, max currMaxRange range,
            min currMin prob, max currMax prob ) (Pass (28<ft> * 12<inch/ft>),Pass 0<inch>,1.,0.)
        |> function 
           | (Pass minRange, Pass maxRange,minProbability,maxProbability) ->
                inch.ToMM(int minRange * 1<inch>), inch.ToMM(int maxRange * 1<inch>),minProbability,maxProbability
           | (Fail _, Pass maxRange,minProbability,maxProbability) ->  
                0<mm>, inch.ToMM(int maxRange * 1<inch>),minProbability,maxProbability  
           | (_, _,minProbability,maxProbability) ->  
                0<mm>, 0<mm>,0.,0.       
    let stopPercent i length = float (i + 1) / float length |> sprintf "%.2f"
    let percentGreen (range:int<mm>) =
        if maxRange - minRange = 0<mm> then "#00FF00"
        else colour <| (1. - (float (range - minRange) / float(maxRange - minRange))) * 255. 

    let stopsPercentGreenAndOpacity = 
        let stops = 
            dist 
            |> List.toArray
            |> Array.mapi (fun i (range,prob) -> 
                match range,prob with 
                | Result(Fail _),_ | _,0.0 -> (stopPercent i length, colour 255.), 0.0
                | Result(Pass (Int(range))),_ -> (stopPercent i length, percentGreen (inch.ToMM(int range * 1<inch>))), prob
                | _ -> failwith "invalid range calculation") 
            |> Array.rev
        stops
        |> Array.scan (fun ((_,_),lastProb) ((stopPercent,green),prob) -> ((stopPercent,green),prob+lastProb)) (("0.00","#000000"), 0.0)                          
        |> List.ofArray 
        |> List.skip 1
        |> List.sortByDescending snd
        |> List.map(fun ((offset:string,stopcolor:string),opacity:float) -> 
                        stop [ Offset !^ offset
                               StopColor stopcolor
                               StopOpacity !^ opacity ] [])
        
    (minRange,maxRange, stopsPercentGreenAndOpacity)
let rec showOperationDistribution f op = 
    match op with 
    | Value v -> 
        match v with 
        | Str s -> 
            b  [] [str s]
        | Dist(d) -> f d
        | v -> 
            GameActions.Primitives.View.unparseValue v
        |> List.singleton 
    
    | ParamArray(OpList(ops)) -> 
        [ section [ClassName "columns"] (List.map (fun op -> div [ClassName "column"] [showOperationDistribution f op]) ops) ]
    | op -> GameActions.Primitives.View.unparse op
    |> div [ClassName "column"]
let showGamePrimitive gp = 
    div [] [GameActions.Primitives.View.unparseValue gp]
let rec showDisplayType dt = 
    let re = match dt with 
             | DInt(i) -> string i |> str
             | DFloat(f) -> sprintf "%.2f" f |> str
             | DNoValue -> "--" |> str
             | DStr s -> s |> str
             | DDist d -> "distribution" |> str
             | DResult(r) -> GameActions.Primitives.View.unparseResult showDisplayType r
    div [] [re]

let showAverages (dist:Distribution<GamePrimitive>) =

    let rec mult (v,probability) =
        match v with 
        | NoValue -> DNoValue
        | Int i -> DFloat ((float i) * probability)
        | Str s -> DStr (sprintf "(%s %.0f%%)" s (100. * probability))
        | Result r -> DResult (Result.map (fun gp -> mult(gp,probability)) r)
        | Dist d -> DDist (d |> List.map (fun (g,p) -> (mult (g,1.)),p*probability)) 
    let averageDistribution = dist |> List.fold (fun sum (v,p) -> mult (v,p) + sum) DNoValue
    showDisplayType averageDistribution
let showProbabilitiesOfActions (dist:Distribution<GamePrimitive>) = 
    let result = 
          dist 
          |> List.groupBy fst
          |> List.map(fun (f,probs) -> f, List.sumBy snd probs)
    match result with 
    | [] -> str ""
    | _ ->  let max = result |> List.maxBy snd |> snd
            let min = result |> List.minBy snd |> snd
            let total = result |> List.sumBy snd 
            result
                |> List.map (fun (r, prob) -> 
                      match r with 
                      | IntResult r -> 
                          let rec passFailToExpectation = function 
                            | Pass _ -> float 0xFF  
                            | Fail _ -> float 0x00 
                            | List xs -> List.averageBy passFailToExpectation xs 
                            | Tuple (x,y) when x = y ->  float 0xFF 
                            | Tuple (x,y) -> (x / (x + y) ) * float 0xFF
                          let greenValue = Result.map float r |> passFailToExpectation
                          let alpha = opacity min max prob
                          div [Style [Color (colourA greenValue alpha)]] [str (printResultD r); str <| sprintf " %.2f%%" (prob / total * 100.)]
                      | v -> div [] [GameActions.Primitives.View.unparseValue v; str <| sprintf " %.2f%%" (prob / total * 100.)])
            |> div [ClassName "column"]            
    

let showSample  = sample >> showGamePrimitive 
    

let groupFor model display = 
      g     [Transform <| sprintf "translate(%f,%f)" model.PosX model.PosY]
            [ g   [ Transform model.Scale ] display ]
let rangeRoot name model =
    let dist = GameActions.Primitives.State.tryFindLabel name model.ProbabilityRules
    let ranges id (min:int<mm>,max:int<mm>,stops) = 
        g [] 
          [   defs  [] 
                    [ radialGradient [ Id id ]
                                       stops ]
              circle [Fill <| sprintf "url(#%s)" id
                      R !^ (float max)] [] ]

    dist
    |> Option.bind(|IsDistribution|_|)
    |> Option.map 
        (rangeStops 
        >> ranges name
        >> List.singleton
        >> groupFor model )
let root model dispatch =
      let modelDisplay = 
            [ circle   [ R !^ (model.Size / 2 |> float) :> IProp
                         OnClick (fun mouseEvt -> Select |> dispatch) :> IProp] []
              text     [ TextAnchor "middle"
                         Y !^ 50.
                         StrokeWidth (!^ ".5")
                         Fill "#000000"
                         Stroke "#000000"
                         FontSize !^ "25"    ] 
                         [ str model.Name ] ]
            //
            
            
      groupFor model modelDisplay

