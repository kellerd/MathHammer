
module Determinism
open Distribution
type Determinism<'a> = NoResult | Deterministic of 'a | NonDeterministic of Distribution<'a>
let fromDistribution = function [a,1.0] -> Deterministic a | [] -> NoResult | det -> NonDeterministic det     
let toDistribution x : Distribution<_> = 
    match x with 
    | NoResult -> []
    | Deterministic a -> always a 
    | NonDeterministic a -> a
let returnM x = Deterministic x
let bind f x = 
    match x with 
    | NoResult -> NoResult
    | Deterministic a -> 
        f a |> toDistribution |> fromDistribution
    | NonDeterministic det -> 
        NonDeterministic (det |> Distribution.bind(f >> toDistribution))
let map f = 
    bind (f >> returnM)