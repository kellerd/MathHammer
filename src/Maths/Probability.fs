module Probability 
    open System
    type Probability = double
    type Distribution<'a> = ('a * Probability) list
    type Event<'a> = 'a -> bool
 
    let printDistribution (v : Distribution<'a>) =
        let negate x = -x
        v |> List.sortBy (snd >> negate) |> List.iter (fun (a,p) -> printfn "%A: %.2f%%" a (p * 100.0))
    let always (a : 'a) : Distribution<'a> =
        [a, 1.0]
    let never (a : 'a) : Distribution<'a> =
        [a, 0.0]
    let expectation (predicate:'a -> float) (v : Distribution<'a>) : float = 
        List.foldBack (fun  (a,prob) acc -> acc + predicate a * prob) v 0.0
    let rnd = System.Random()
    let sample (v : Distribution<'a>)  = 
        let prob = rnd.NextDouble()
        v |> Seq.scan (fun (p,acc) (n,prob) -> Some n, acc + prob )  (None, 0.0) 
        |> Seq.pairwise
        |> Seq.pick(fun ((_,p1),(a,p2)) -> if prob >= p1 && prob < p2 then a else None)
    let uniformDistribution (ls : 'a list) =
        let ws = 1.0 / float (List.length ls)
        List.map (fun l -> (l, ws)) ls
    let calcProbabilityOfEvent (e : Event<'a>) (vs : Distribution<'a>) : Probability =
        vs |> List.filter (fst >> e)
           |> List.sumBy snd
 
    let (>?) a b = calcProbabilityOfEvent b a


    [<AutoOpen>]
    module internal Operations =
 
        let normalize (v : Distribution<'a>) =
            let dict = new System.Collections.Generic.Dictionary<_,_>()
            let get a = if dict.ContainsKey a then dict.[a] else 0.0
            let add (a,p) = dict.[a] <- get a + p
            v |> List.iter add
            let totVal = Seq.sum dict.Values
            dict |> Seq.map (fun kvp -> 
                    if totVal > 1.0 then (kvp.Key, kvp.Value / totVal)
                    else (kvp.Key, kvp.Value))
                 |> List.ofSeq
            
        let coinFlip (pFirst : float) (d1 : Distribution<'T>) (d2 : Distribution<'T>) = 
            if pFirst < 0.0 || pFirst > 1.0 then failwith "invalid probability in coinFlip"
            let d1' = d1 |> List.map (fun (a,prob) -> a,prob * pFirst) 
            let d2' = d2 |> List.map (fun (b,prob) -> b,prob * (1.0 - pFirst)) 
            let combined = List.append d1' d2' 
            combined |> normalize
        let either (d1 : Distribution<'T>) (d2 : Distribution<'T>) = coinFlip 0.5 d1 d2
        let combine  (vs : Distribution<'T> seq) : Distribution<'T> = List.concat vs |> normalize

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

            

    [<AutoOpen>]
    module Monad =
        let returnM (a : 'a) : Distribution<'a> =
            always a
 
        let bind (f : 'a -> Distribution<'b>) (v : Distribution<'a>) : Distribution<'b> =
            [ for (a,p) in v do
              for (b,p') in f a do
              yield (b, p*p')
            ] |> normalize
        
        let (>>=) x f = bind f x
 
        type DistrBuilder () =
            member x.Bind(m, f) = m >>= f
            member x.Return(v) = returnM v
            member x.ReturnFrom(v) = v
            member x.Delay(f) = f ()
 
    let dist= Monad.DistrBuilder()   
    let map f = bind (f >> returnM) 
    let apply f v = 
        dist{
            let! v' = v
            let! f' = f
            return f' v'
        }

    let rec traverseResultM f list =
        // define a "cons" function
        let cons head tail = head :: tail

        // loop through the list
        match list with
        | [] -> 
            // if empty, lift [] to a Result
            returnM []
        | head::tail ->
            // otherwise lift the head to a Result using f
            // then lift the tail to a Result using traverse
            // then cons the head and tail and return it
            f head                 >>= (fun h -> 
            traverseResultM f tail >>= (fun t ->
            returnM (cons h t) ))


    let rec takeN (v : Distribution<'a>) (n : int) : Distribution<'a list> =
        dist{
            if n <= 0 then return [] else
            let! wert = v
            let! rest = takeN v (n-1)
            return (wert::rest)
        }
    let permute d1 d2 = 
        dist{
            let! d1' = d1
            let! d2' = d2
            return d1', d2'
        }

    module Example =
        let singleDice : Distribution<int> = uniformDistribution [1..6] 
        let ``Point 5 expectation`` = 
            singleDice |> expectation (function | 1 | 2 | 3 -> 1.0 | 4 | 5 | 6 -> 0.0 | _ -> 0.0)
        let ``Average dice roll`` = 
            singleDice |> expectation float

        let nDice = takeN singleDice

        let ``Average 3D6`` = 
            dist{
                let! threeDice = nDice 3
                let sum = threeDice |> List.sum
                return sum            
            } |> expectation float


        
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
                diceValues |> List.filter ((=) 6)
                    |> (fun l -> List.length l >= 2)
            nDice 4 >? gotTwoSixes
 
        let ``probability you get out in parcheesi`` =
            dist{
                let! w1 = singleDice
                if w1 = 6 then return "you made it" else
                let! w2 = singleDice
                if w2 = 6 then return "you made it" else
                let! w3 = singleDice
                if w3 = 6 then return "you made it" else
                return "fuck this game"
            }
      
        let ``Losses when playing with 3 attack dice vs 2 defence dice in Risiko game`` =
            dist{
                let! offensive = nDice 3
                let! defensive = nDice 2
                let defensivLosses =
                    List.zip (offensive |> List.sort |> List.tail)
                           (defensive |> List.sort)
                    |> List.sumBy (fun (o,d) -> if d >= o then 0 else 1)
                return sprintf "%d:%d" (2-defensivLosses) defensivLosses
            } |> printDistribution    