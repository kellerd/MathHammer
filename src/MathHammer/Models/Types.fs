module MathHammer.Models.Types


let rnd = System.Random()
let coinFlip (p : float) (d1 : Distribution<'T>) (d2 : Distribution<'T>) =

    if p < 0.0 || p > 1.0 then failwith "invalid probability in coinFlip"

    { new Distribution<'T> with
    member d.Sample =
        if rnd.NextDouble() < p then d1.Sample else d2.Sample
    member d.Support = Set.union d1.Support d2.Support
    member d.Expectation(H) =
        p * d1.Expectation(H) + (1.0 - p) * d2.Expectation(H) }

let bind (dist : Distribution<'T>) (k : 'T -> Distribution<'U>) =
    { new Distribution<'U> with
    member d.Sample = (k dist.Sample).Sample
    member d.Support = Set.unionMany (dist.Support |> Set.map (fun d -> (k d).Support))
    member d.Expectation H = dist.Expectation(fun x -> (k x).Expectation H) }

type DistributionBuilder() =
    member x.Delay f = bind (always ()) f
    member x.Bind(d, f) = bind d f
    member x.Return v = always v
    member x.ReturnFrom vs = vs

let dist = new DistributionBuilder()

let weightedCases (inp : ('T * float) list) =
    let rec coinFlips w l =
        match l with
        | [] -> failwith "no coinFlips"
        | [(d, _)] -> always d
        | (d, p) :: rest -> coinFlip (p / (1.0 - w)) (always d) (coinFlips (w + p) rest)
    coinFlips 0.0 inp
let countedCases inp =
    let total = Seq.sumBy (fun (_, v) -> v) inp
    weightedCases (inp |> List.map (fun (x, v) -> (x, float v / float total)))
let evenDistribution inp = 
    inp |> List.map (fun x -> 1,x) |> countedCases

type SequenceItem<'a> = 
    | Absolute of 'a

type Die =
    | D3
    | D6



type GamePrimitive = 
    | Dice of Die list
    | Value of int
    | DPlus of int 
    | NoValue 
type Action = 
    | Characteristic of GamePrimitive
    | Ability of GamePrimitive
let isCharacteristic = function Characteristic x -> true | Ability x -> false


type Model = {posX:float; posY:float; name:string; attributes:list<string*Action>; }


type Msg = 
    | ChangePosition of float * float
    | Select
