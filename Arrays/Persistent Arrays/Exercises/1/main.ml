(*Implementation of persistent arrays | march 20 2025*)

(*Signature*)
module type PersistentArray = sig 
  type 'a t
  val init: int -> (int -> 'a) -> 'a t
  val length: 'a t -> int
  val get: 'a t -> int -> 'a
  val set: 'a t -> int -> 'a -> 'a t
  val iteri: (int -> 'a -> unit) -> 'a t -> unit
end

(*Implementation*)
type 'a t = 'a data ref
and 'a data =
  | Arr of 'a array
  | Diff of int * 'a * 'a t
let init n f = ref (Arr(Array.init n f))
let rec reroot pa = match !pa with
  | Arr a -> a
  | Diff (i, v, pa') -> 
    let a = reroot pa' in
    match !pa' with
    | Arr _ -> pa := Arr a; a
    | _ ->
      let old = a.(i) in
      a.(i) <- v;
      pa := Arr a;
      pa' := Diff (i, old, pa);
      a
let length pa = Array.length (reroot pa)
let get pa i = (reroot pa).(i)
let iteri f pa = Array.iteri f (reroot pa)
let set pa i v: 'a t = 
  match !pa with 
  | Arr a -> 
    a.(i) <- v;
    let res = ref (Arr a) in
    res
  | Diff _ ->
    let a = reroot pa in
    let old = a.(i) in
    a.(i) <- v;
    let res = ref (Arr a) in
    pa := Diff (i, old, res);
    res