(*
 here we represent a graph in which each vertex is associated with the set of its successors
*)
module Graph = struct
  module H = Hashtbl 
  module V = H (*main hashtable*)
  type vertex = int

  module E = Hashtbl (*Sets of the adjacent vertices *)
  type t = (int, (int, unit) E.t) V.t (* the outer hashtble maps vertices to another hashtbl*)

  let create () = V.create
  let nb_vertex g = V.length g

  let mem_vertex g v = V.mem g v
  let add_vertex g v = if not (mem_vertex g v) then V.add g v (E.create)
  let remove_vertex g v = 
    if mem_vertex g v then
      V.remove g v;
      V.iter (fun _ s -> E.remove s v) g
  let mem_edge g v1 v2 = E.mem(V.find g v1) v2
  let add_edge g v1 v2 = E.replace (V.find g v1) v2
  let remove_edge g v1 v2 = E.remove(V.find g v1) v2

  let iter_vertex f g = V.iter (fun v _ -> f v) g
  let iter_succ f g v = E.iter (fun w _ -> f w) (V.find g v)
  let iter_edge f g = V.iter (fun v s -> E.iter (f v) s) g

end