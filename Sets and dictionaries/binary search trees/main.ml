(*Minimal signatures*)
module type PersistentSet = sig
  type elt (*represents the elements in the set*)
  type t (*represents the tree itself*)
  val empty: t (*represents the empty set*)
  val add: elt -> t -> t
  val mem: elt -> t -> bool
  val min_elt: t -> elt
  val remove: elt -> t -> t
  val cardinal: t -> int
  val merge: t -> t -> t
end

module type Ordered = sig
  type t 
  val compare: t -> t -> int
end

(*implementation*)

module Make(X: Ordered) : PersistentSet with type elt = X.t = struct
  type elt = X.t
  type t = Empty | Node of t * elt * t
  let empty = Empty

  let rec min_elt = function
  | Empty -> raise Not_found
  | Node (Empty, v, _) -> v
  | Node (l, _, _) -> min_elt l

  let rec mem x = function
  | Empty -> false
  | Node (l,v,r) -> 
    let c = X.compare x v in
    c = 0 || if c < 0 then mem x l else mem x r
  
  let rec add x t = 
    match t with 
    | Empty -> Node (Empty, x, Empty)
    | Node (l,v,r) ->
      let c = X.compare x v in
      if c = 0 then t
      else if c < 0 then Node (add x l, v, r)
      else Node (l, v, add x r)

  let rec remove_min_elt = function
  | Empty -> Empty
  | Node (Empty, _, r) -> r
  | Node (l,v,r) -> Node (remove_min_elt l,v,r)

  let merge t1 t2 = match t1, t2 with
  | Empty, t | t, Empty -> t
  | _ -> Node (t1, min_elt t2, remove_min_elt t2)
  
  let rec remove x t =
    match t with
    | Empty -> Empty
    | Node (l,v,r) ->
      let c = X.compare x v in
      if c = 0 then merge l r
      else if c < 0 then merge l r
      else if c < 0 then Node (remove x l, v, r)
      else Node (l, v, remove x r)
  
  let rec cardinal = function
  | Empty -> 0
  | Node (l, _, r) -> 1 + cardinal l + cardinal r
end