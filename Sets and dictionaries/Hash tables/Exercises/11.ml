exception MaxLengthExceded
type elt
type t = {
  mutable buckets: (elt list) array;
  mutable size : int;
}
let hash (table: elt): int = 0 (*dummy hash function*)
let resize h = 
  let n = Array.length h.buckets in
  let m = min (2 * n) (Sys.max_array_length) in
  let a = Array.make m [] in
  let rehash x =
    let i = (hash x land max_int) mod m in
    a.(i) <- x :: a.(i)
  in
  Array.iter (List.iter rehash) h.buckets;
  h.buckets <- a;
  raise MaxLengthExceded