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
type t = vertex list array
let create n = Array.make n []
let nb_vertex = Array.length
let mem_edge g v1 v2 =
  List.mem v2 g.(v1)
let add_edge g v1 v2 =
  if not (mem_edge g v1 v2) then g.(v1) <- v2 :: g.(v1)
let remove_edge g v1 v2 =
  if (mem_edge g v1 v2) then g.(v1) <- List.filter ((<>) v2) g.(v1)
let iter_succ f g v =
  List.iter f g.(v)
let iter_edge f g =
  for v = 0 to nb_vertex g - 1 do List.iter f g.(v) done
