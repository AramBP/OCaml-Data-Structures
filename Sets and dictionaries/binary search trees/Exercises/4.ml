type elt
type t = Empty | Node of t * elt * t
let empty = Empty

let rec floor (x: elt) (tree: t): elt = 
  match tree with
  | Empty -> raise Not_found
  | Node (Empty, v, Empty) -> v
  | Node (l, v, r) -> 
    if x < v then floor x l 
    else if x > v then try floor x r with Not_found -> v 
    else v