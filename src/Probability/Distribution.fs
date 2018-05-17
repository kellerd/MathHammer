[<RequireQualifiedAccess>]
module Distribution

let normalize v =
    let total = v |> Seq.sumBy snd
    v
    |> Seq.groupBy fst
    |> Seq.map (fun (k, vals) -> 
           if total > 1.0 then k, Seq.sumBy snd vals / total
           else k, Seq.sumBy snd vals)
    |> Seq.toList

type Probability = double

[<CustomEquality; CustomComparison>]
type Distribution<'a when 'a : equality and 'a : comparison> =
    { Probabilities : ('a * Probability) list }
    
    member v.Normalize : Distribution<_> =
        let norm = v.Probabilities |> normalize
        { v with Probabilities = norm }
    
    override x.GetHashCode() = hash (x.Normalize).Probabilities
    
    override x.Equals(obj) =
        match obj with
        | :? Distribution<'a> as y -> 
            let convert = List.sortBy fst >> List.map (fun (a, b) -> a, System.BitConverter.DoubleToInt64Bits(b))
            let x' = (x.Normalize).Probabilities |> convert
            let y' = (y.Normalize).Probabilities |> convert
            if x'.Length = y'.Length then List.zip x' y' |> List.forall (fun ((a, p), (b, p2)) -> a = b && abs (p - p2) < 5L)
            else false
        | _ -> false
    
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Distribution<'a> as y -> compare x.Probabilities y.Probabilities
            | _ -> invalidArg "yobj" "cannot compare values of different types"

type Event<'a> = 'a -> bool

let create probabilities = { Probabilities = normalize probabilities }

let printDistribution (v : Distribution<_>) =
    let negate x = -x
    v.Probabilities
    |> List.sortBy (snd >> negate)
    |> List.iter (fun (a, p) -> printfn "%A: %.2f%%" a (p * 100.0))

let always (a : 'a) : Distribution<_> = create [ a, 1.0 ]
let never (a : 'a) : Distribution<_> = create [ a, 0.0 ]
let expectation (predicate : 'a -> float) (v : Distribution<_>) : float =
    List.foldBack (fun (a, prob) acc -> acc + predicate a * prob) v.Probabilities 0.0
let rnd = System.Random()

let sample (v : Distribution<_>) =
    let prob = rnd.NextDouble()
    v.Probabilities
    |> Seq.scan (fun (_, acc) (n, prob) -> Some n, acc + prob) (None, 0.0)
    |> Seq.pairwise
    |> Seq.pick (fun ((_, p1), (a, p2)) -> 
           if prob >= p1 && prob < p2 then a
           else None)

let uniformDistribution (ls : 'a list) : Distribution<_> =
    let ws = 1.0 / float (List.length ls)
    List.map (fun l -> (l, ws)) ls |> create

let calcProbabilityOfEvent (e : Event<'a>) (vs : Distribution<_>) : Probability =
    vs.Probabilities
    |> List.filter (fst >> e)
    |> List.sumBy snd

let (>?) a b = calcProbabilityOfEvent b a

let coinFlip (pFirst : float) (d1 : Distribution<'T>) (d2 : Distribution<'T>) =
    if pFirst < 0.0 || pFirst > 1.0 then failwith "invalid probability in coinFlip"
    let d1' = d1.Probabilities |> List.map (fun (a, prob) -> a, prob * pFirst)
    let d2' = d2.Probabilities |> List.map (fun (b, prob) -> b, prob * (1.0 - pFirst))
    let combined = List.append d1' d2'
    combined |> create

let either (d1 : Distribution<'T>) (d2 : Distribution<'T>) = coinFlip 0.5 d1 d2

let combine (vs : Distribution<'T> seq) : Distribution<'T> =
    Seq.collect (fun v -> v.Probabilities) vs
    |> List.ofSeq
    |> create

let weightedCases (inp : ('T * float) list) =
    let rec coinFlips w l =
        match l with
        | [] -> failwith "no coinFlips"
        | [ (d, _) ] -> always d
        | (d, p) :: rest -> coinFlip (p / (1.0 - w)) (always d) (coinFlips (w + p) rest)
    coinFlips 0.0 inp

let countedCases inp =
    let total = Seq.sumBy (fun (_, v) -> v) inp
    weightedCases (inp |> List.map (fun (x, v) -> (x, v / total)))

let returnM (a : 'a) : Distribution<_> = always a

let bind (f : 'a -> Distribution<'b>) (v : Distribution<_>) : Distribution<'b> =
    [ for (a, p) in v.Probabilities do
          for (b, p') in (f a).Probabilities do
              yield (b, p * p') ]
    |> create

let unzip (v : Distribution<'a * 'b>) : Distribution<'a> * Distribution<'b> =
    let (a, b) =
        v.Probabilities
        |> List.map (fun ((a, b), p) -> (a, p), (b, p))
        |> List.unzip
    { Probabilities = a }, { Probabilities = b }

let (>>=) x f = bind f x

type DistrBuilder() =
    member __.Bind(m, f) = m >>= f
    member __.Return(v) = returnM v
    member __.ReturnFrom(v) = v
    member __.Delay(f) = f()

let dist = DistrBuilder()
let map f = bind (f >> returnM)
let mapProbility f (x : Distribution<_>) : Distribution<_> = List.map (fun (a, p) -> a, f p) x.Probabilities |> create
let multiplyProbability p = mapProbility ((*) p)

let groupBy projection mapping (x : Distribution<'a>) : Distribution<'a> =
    List.groupBy (fst >> projection) x.Probabilities
    |> List.choose (function 
           | (_, []) -> None
           | (_, h :: tail) -> Some <| List.fold (fun (c, p) (n, p2) -> mapping c n, (p + p2)) h tail)
    |> create

let traverseResultM f list =
    // define a "cons" function
    let cons head tail = head :: tail
    // right fold over the list
    let initState = returnM []
    let folder head tail = f head >>= (fun h -> tail >>= (fun t -> returnM (cons h t)))
    List.foldBack folder list initState

let get d = d.Probabilities
let values d = d.Probabilities |> List.map fst
let choose f (xs : Distribution<_>) : Distribution<_> =
    List.choose (fun (a, p) -> f a |> Option.map (fun a' -> a', p)) xs.Probabilities |> create
let sequenceResultM x = traverseResultM id x

let rec takeN (v : Distribution<_>) (n : int) : Distribution<'a list> =
    dist { 
        if n <= 0 then return []
        else let! wert = v
             let! rest = takeN v (n - 1)
             return (wert :: rest)
    }

let pair d1 d2 = dist { let! d1' = d1
                        let! d2' = d2
                        return d1', d2' }

let cartesian (xs : Distribution<'a>) (ys : Distribution<'b>) : Distribution<'a * 'b> =
    xs.Probabilities
    |> List.collect (fun (x, px) -> ys.Probabilities |> List.map (fun (y, py) -> (x, y), (px * py)))
    |> create

module Example =
    let singleDice : Distribution<int> = uniformDistribution [ 1..6 ]
    
    let ``Point 5 expectation`` =
        singleDice |> expectation (function 
                          | 1 | 2 | 3 -> 1.0
                          | 4 | 5 | 6 -> 0.0
                          | _ -> 0.0)
    
    let ``Average dice roll`` = singleDice |> expectation float
    let nDice n = takeN singleDice n
    
    let ``Average 3D6`` =
        dist { 
            let! threeDice = nDice 3
            let sum = threeDice |> List.sum
            return sum
        }
        |> expectation float
    
    // either (always 1) (always 2) |> either (always 3)
    // combine [(uniformDistribution [1..6] );(uniformDistribution [1..3])]
    // uniformDistribution [1;2;3;4;5;6;1;2;3] |> normalize
    // [always 1; always 2;always 3 ] |> combine
    let threeDice = nDice 3
    // threeDice |> sample
    let x = bind nDice (always 3)
    let y = bind (List.sum >> always) x
    
    let z =
        (always 3)
        >>= nDice
        >>= (List.sum >> always)
        |> expectation float
    
    let ``probability you get at least two sixes when throwing 4 dice`` =
        let gotTwoSixes (diceValues : int list) : bool =
            diceValues
            |> List.filter ((=) 6)
            |> (fun l -> List.length l >= 2)
        nDice 4 >? gotTwoSixes
    
    let ``probability of 3 attacks at 3+`` =
        let gotN n (diceValues : int list) : bool =
            diceValues
            |> List.filter (fun n -> n >= 3)
            |> (fun l -> List.length l = n)
        
        let test n = (n, nDice 3 >? gotN n)
        test 0, test 1, test 2, test 3
    
    let ``probability you get out in parcheesi`` =
        dist { 
            let! w1 = singleDice
            if w1 = 6 then return "you made it"
            else 
                let! w2 = singleDice
                if w2 = 6 then return "you made it"
                else 
                    let! w3 = singleDice
                    if w3 = 6 then return "you made it"
                    else return "fuck this game"
        }
    
    let ``Losses when playing with 3 attack dice vs 2 defence dice in Risiko game`` =
        dist { 
            let! offensive = nDice 3
            let! defensive = nDice 2
            let defensivLosses =
                List.zip (offensive
                          |> List.sort
                          |> List.tail) (defensive |> List.sort)
                |> List.sumBy (fun (o, d) -> 
                       if d >= o then 0
                       else 1)
            return sprintf "%d:%d" (2 - defensivLosses) defensivLosses
        }
