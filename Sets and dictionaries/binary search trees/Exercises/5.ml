exception DoneIterating
type elt
type t = Empty | Node of t * elt * t
let empty = Empty

let rec iter(f: elt -> unit) (tree: t): unit =
  match tree with
  | Empty -> ()
  | Node (l, v, r) ->
    iter f l;
    f v;
    iter f r;

