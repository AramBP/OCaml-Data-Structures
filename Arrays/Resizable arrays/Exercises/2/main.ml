exception MaximumArrayLengthExceded
module type ResizableArray = sig
  type 'a t
  val length: 'a t -> int
  val make: int -> 'a -> int option -> 'a t 
  val resize: 'a t -> int -> unit
  val get: 'a t -> int -> 'a
  val set: 'a t -> int -> 'a -> unit
end

module Arr: ResizableArray = struct
  type 'a t = {
    default: 'a;
    increment: int option;
    mutable size: int;
    mutable data: 'a array;
  }

  let length (a: 'a t): int = a.size
  let make (n: int) (d: 'a) (i: int option): 'a t = { default = d; size = n; data = Array.make n d; increment = i}
  let get a i: 'a = if i < 0 || i >= a.size then invalid_arg "get"; a.data.(i)
  let set a i v = if i < 0 || i >= a.size then invalid_arg "set"; a.data.(i) <- v
  
  let resize (a: 'a t) (s: int) =
    if s >= Sys.max_array_length then begin
      Printf.printf "max_array_length exceded\n Only set length from %d to %d\n" a.size (Sys.max_array_length-1);
      let a' = Array.make (Sys.max_array_length - 1) a.default in
      Array.blit a.data 0 a' 0 a.size;
      a.data <- a';
      a.size <- Sys.max_array_length-1
    end
    else if s <= a.size then begin 
      Array.fill a.data s (a.size - s) a.default;
      a.size <- s
    end
    else begin 
      match a.increment with
      | None ->
        let n = Array.length a.data in
        if s > n then begin 
          let n' = max (2 * n) s in
          let a' = Array.make n' a.default in
          Array.blit a.data 0 a' 0 a.size;
          a.data <- a';
          a.size <- s
        end
      | Some n -> 
        let data_size = Array.length a.data in
        let n'  = max (n+data_size) s in
        let a' = Array.make n' a.default in
        Array.blit a.data 0 a' 0 a.size;
        a.data <- a';
        a.size <- s
    end;

    let data_size = Array.length a.data in
    if (a.size < data_size/4 && data_size > 1) then begin 
      let n' = max (data_size/2) 1 in
      let a' = Array.make n' a.default in
      Array.blit a.data 0 a' 0 a.size;
      a.data <- a'
    end;

end