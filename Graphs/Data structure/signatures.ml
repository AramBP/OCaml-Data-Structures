(* Integer vertices *)
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