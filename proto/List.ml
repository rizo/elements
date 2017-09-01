
(* open Kernel

type 'a t = 'a list

let cons x xs = x :: xs

let compare ?(cmp = Pervasives.compare) a b =
  let rec loop a b =
    match a, b with
    | [], [] -> Comparable.equal
    | [], _  -> Comparable.less
    | _ , [] -> Comparable.greater
    | x :: xs, y :: ys ->
      let n = cmp x y in
      if n = Comparable.equal
      then loop xs ys
      else n in
  loop a b
 *)
