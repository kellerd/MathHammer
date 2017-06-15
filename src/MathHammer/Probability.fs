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

    let expectation (predicate:'a -> float) (v : Distribution<'a>) : float = 
        let total = List.foldBack (fun  (a,prob) acc -> acc + predicate a ) v 0.0
        total / (List.length v |> float)
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
            dict |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
                 |> List.ofSeq
 
    [<AutoOpen>]
    module Monad =
        let returnM (a : 'a) : Distribution<'a> =
            always a
 
        let bind (v : Distribution<'a>) (f : 'a -> Distribution<'b>) : Distribution<'b> =
            [ for (a,p) in v do
              for (b,p') in f a do
              yield (b, p*p')
            ] |> normalize
        let (>>=) f m =
            bind f m
 
        type DistrBuilder () =
            member x.Bind(m, f) = m >>= f
            member x.Return(v) = returnM v
            member x.ReturnFrom(v) = v
            member x.Delay(f) = f ()
 
    let distr = Monad.DistrBuilder()   
    let map f v = bind v (f >> returnM)
    let apply f v = 
        distr {
            let! v' = v
            let! f' = f
            return f' v'
        }
    let rec takeN (v : Distribution<'a>) (n : int) : Distribution<'a list> =
        distr {
            if n <= 0 then return [] else
            let! wert = v
            let! rest = takeN v (n-1)
            return (wert::rest)
        }
 
    module Example =
      let singleDice : Distribution<int> =
          uniformDistribution [1..6]
      let ``Point 5 expectation`` = 
        singleDice |> expectation (function | 1 | 2 | 3 -> 1.0 | 4 | 5 | 6 -> 0.0)
      let ``Average dice roll`` = 
        singleDice |> expectation float

      let nDice = takeN singleDice
      
      let ``Average 3D6`` = 
        distr {
            let! threeDice = nDice 3
            let sum = threeDice |> List.sum
            return sum            
        } |> expectation float


      
      let threeDice = nDice 3
      threeDice |> sample
      let x = bind (always 3) nDice 
      let y = bind x (List.sum >> always) 

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
          distr {
              let! w1 = singleDice
              if w1 = 6 then return "you made it" else
              let! w2 = singleDice
              if w2 = 6 then return "you made it" else
              let! w3 = singleDice
              if w3 = 6 then return "you made it" else
              return "fuck this game"
          }
      
      let ``Losses when playing with 3 attack dice vs 2 defence dice in Risiko game`` =
          distr {
              let! offensive = nDice 3
              let! defensive = nDice 2
              let defensivLosses =
                  List.zip (offensive |> List.sort |> List.tail)
                           (defensive |> List.sort)
                  |> List.sumBy (fun (o,d) -> if d >= o then 0 else 1)
              return sprintf "%d:%d" (2-defensivLosses) defensivLosses
          } |> printDistribution    