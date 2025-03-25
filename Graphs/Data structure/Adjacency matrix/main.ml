module type Graph = sig
  type vertex = int
  type t
  val create: int -> t
  val nb_vertex: t -> int
  val mem_edge: t -> vertex -> vertex -> bool
  val add_edge: t -> vertex -> vertex -> unit
  val remove_edge : t -> vertex -> vertex -> unit
  val iter_succ: (vertex -> unit) -> t -> vertex -> unit
  val iter_edge: (vertex -> vertex -> unit) -> t -> unit
end

type vertex = int
type t = bool array array
let create n = Array.make_matrix n n false
let nb_vertex = Array.length
let mem_edge g v1 v2 = g.(v1).(v2)
let add_edge g v1 v2 = g.(v1).(v2) <- true
let remove_edge g v1 v2 = g.(v1).(v2) <- false
let iter_succ (f: vertex -> unit) (g: t) (v: vertex): unit =
  Array.iteri (fun w b -> if b then f w) g.(v)
let iter_edge (f: vertex -> vertex -> unit) (g: t) : unit =
  for v = 0 to ((nb_vertex g) - 1) do iter_succ (f v) g v done 