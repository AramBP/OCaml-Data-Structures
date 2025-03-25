(*
((abs (X.hash x)) mod (Array.length h.buckets))
is a bad solution since hash function are often deterministic. That is they will
generate a unique hash value  based on the input value. By taking the absolute value of the
output we could get two unique input values mapped to the same hash value
*)