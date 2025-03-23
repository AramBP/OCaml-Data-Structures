(* calculate height of a binary search tree*)
type elt
type t = Empty | Node of t * elt * t
let empty = Empty
let rec height (t: t) : int = 
  match t with
    | Empty -> 0
    | Node (l, _, r) -> 1 + max (height l) (height r)