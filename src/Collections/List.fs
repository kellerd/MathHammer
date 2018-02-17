module List 
let reduceSafe empty f list = 
  match list with
  | [] -> empty
  | head::tail -> List.fold f head tail