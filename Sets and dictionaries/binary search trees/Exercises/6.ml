type elt
type t = Empty | Node of t * elt * t
let empty = Empty

let rec elements (tree: t): elt list =
  match tree with
  | Empty -> []
  | Node (l, v, r) -> (elements l) @ (v::elements r) 
    
