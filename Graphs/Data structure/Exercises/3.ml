type vertex = int
type label = 
| Empty
| Char

type arc = {mutable succ: bool; mutable lbl: label}
type t = {matrix: arc array array; mutable nb_edges: int}

let create n = Array.make_matrix n n {succ = false; lbl = Empty}

let nb_vertex = Array.length

let mem_edge (g: t) (v1: vertex) (v2: vertex) = g.matrix.(v1).(v2).succ
let add_edge (g: t) (v1: vertex) (v2: vertex) (lbl: label) = 
  if not (mem_edge g v1 v2) then 
    g.matrix.(v1).(v2) <- {succ = true; lbl = lbl}; 
    g.nb_edges <- g.nb_edges + 1

let remove_edge (g: t) (v1: vertex) (v2: vertex) = 
  if (mem_edge g v1 v2) then
    g.matrix.(v1).(v2) <- {succ = false; lbl = Empty};
    g.nb_edges <- g.nb_edges -1

let iter_succ (f: vertex -> unit) (g: t) (v: vertex): unit =
  Array.iteri (fun w b -> if b.succ then f w) g.matrix.(v)

let iter_edge (f: vertex -> vertex -> unit) (g: t) : unit =
  for v = 0 to ((nb_vertex g.matrix) - 1) do iter_succ (f v) g v done 

let reverse (g: t): arc array array =
  let n = nb_vertex g.matrix in
  let gt = create n in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      gt.(i).(j) <- g.matrix.(j).(i);
    done
  done;
  gt

