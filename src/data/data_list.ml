
module Iter = Data_iter

open Base

type 'a t = 'a list

let cons = Base.cons

let compare ?(cmp = Base.compare) a b =
  let rec loop a b =
    match a, b with
    | [], [] -> EQ
    | [], _  -> LT
    | _ , [] -> GT
    | x :: xs, y :: ys ->
      let n = cmp x y in
      if n = EQ
      then loop xs ys
      else n in
  loop a b


(* Iterable instance *)

let rec iter self =
  let next s =
    match s with
    | [] -> None
    | x :: xs -> Some (x, xs) in
  Iter (self, next)

include Iter.Input.Make(struct
    type nonrec 'a t = 'a t
    let iter = iter
  end)

