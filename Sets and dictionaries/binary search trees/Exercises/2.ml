type elt
type t = Empty | Node of t * elt * t
let rec cardinal = function
| Empty -> 0
| Node (l, _, r) -> 1 + cardinal l + cardinal r