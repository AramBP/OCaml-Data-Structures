(*signatures*)
module type PersistentSet = sig
  type t
  type elt
  val height: t -> int
  val node: t -> elt -> t -> t
  val min_elt: t -> elt
  val mem: elt -> t -> bool
  val balance: t -> elt -> t -> t
  val add: elt -> t -> t
  val remove_min_elt: t -> t
  val merge: t -> t -> t
  val remove: elt -> t -> t  
end

module type Ordered = sig
  type t (*represents an element of the tree*)
  val compare: t -> t -> int
end
  

(* implementation *)
module Make (X: Ordered): PersistentSet with type elt = X.t = struct
  type elt = X.t
  type t = Empty | Node of t * elt * t * int (*last argument stores the the height of the tree*)

  let height = function
  | Empty -> 0
  | Node (_, _, _, h) -> h

  (* create a new node *)
  let node l v r =
    Node (l, v, r, 1 + max (height l) (height r))

  (* identical to the min_elt function of a normal tree*)
  let rec min_elt = function
  | Empty -> raise Not_found
  | Node (Empty, v, _, _) -> v
  | Node (l, _, _, _) -> min_elt l

  let rec mem x = function
  | Empty -> raise Not_found
  | Node (l,v,r,_) ->
    let c = X.compare x v in
    c = 0 || if c < 0 then mem x l else mem x r

  let balance (l: t) (v: elt) (r: t): t = 
    let hl = height l in
    let hr = height r in
    if hl > hr + 1 then begin
      match l with
      | Node (ll, lv, lr, _) when height ll >= height lr -> node ll lv (node lr lv r)
      | Node (ll, lv, Node (lrl, lrv, lrr, _), _) -> node (node ll lv lrl) lrv (node lrr v r)
      | _ -> assert false
    end
    else if hr > hl + 1 then begin 
      match r with
      | Node (rl, rv, rr, _) when height rr >= height rl -> node (node l v rl) rv rr
      | Node (Node (rll, rlv, rlr, _), rv, rr, _) -> node (node l v rll) rlv (node rlr rv rr)
      | _ -> assert false
    end else node l v r

  let rec add x = function
  | Empty -> Node (Empty, x, Empty, 1)
  | Node (l, v, r, _) as t -> 
    let c = X.compare x v in
    if c = 0 then t
    else if c < 0 then balance (add x l) v r
    else balance l v (add x r)

  let rec remove_min_elt = function
  | Empty -> Empty
  | Node (Empty, _, r, _) -> r
  | Node (l,v,r,_) -> balance (remove_min_elt l) v r 

  let merge t1 t2 = 
    match t1, t2 with
    | Empty, t | t, Empty -> t
    | _ -> balance t1 (min_elt t2) (remove_min_elt t2)

  let rec remove x = function
  | Empty -> Empty
  | Node (l,v,r, _) ->
    let c = X.compare x v in
    if c = 0 then merge l r
    else if c < 0 then balance (remove x l) v r
    else balance l v (remove x r)
end