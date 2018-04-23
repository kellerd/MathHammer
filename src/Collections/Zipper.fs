[<AutoOpen>]
module Zipper 
type Zipper<'a> = 
    | Empty 
    | Zipper of ('a list * 'a * 'a list) 
let ofList l =
    match l with
    | [] -> Empty
    | h :: t -> Zipper ([], h, t)

let right z =
    match z with
    | Empty -> Empty
    | Zipper(l, z, []) -> Zipper(l, z, [])
    | Zipper(l, z, h::rt) -> Zipper(z::l, h, rt)

let left z =
    match z with
    | Empty -> Empty
    | Zipper([], z, r) -> Zipper([], z, r)
    | Zipper(h::lt, z, r) -> Zipper(lt, h, z::r)

let update x z  =
    match z with 
    | Empty -> Empty
    | Zipper(l, _, r) -> Zipper(l, x, r)  

let permute l =     
    let arr = l |> List.toArray
    let len = Array.length arr
    arr 
    |> Array.mapi (fun i a ->       
      let (l,r) = 
        match i with 
        | 0              -> [||],         arr.[i + 1..len - 1] 
        | i when i = len -> Array.rev arr.[0..i-1], [||]
        | _ ->              Array.rev arr.[0..i-1], arr.[i + 1..len - 1] 
      Zipper(List.ofArray l,a,List.ofArray r) ) 
    |> List.ofArray

let toList z = 
    match z with 
    | Empty -> []
    | Zipper(l,z,r) -> (List.rev l) @ (z::r)
let map f z = 
    match z with 
    | Empty -> Empty
    | Zipper(l,z,r) -> Zipper (List.map f l, f z, List.map f r)

//[0..5] |> ofList |> right |> right |> left |> right |> right   |> toList