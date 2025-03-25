type elt
type t = Empty | Node of t * elt * t
let empty = Empty

let rec max_elt = function
| Empty -> raise Not_found
| Node (_, v, Empty) -> v
| Node (_,_,r) -> max_elt r
