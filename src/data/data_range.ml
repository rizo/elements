
module Iter = Data_iter
open Base

type t = Range of (int * int)

let init n m = Range (n, m)

let start (Range (n, _)) = n
let stop  (Range (_, m)) = m

let iter self =
  let next stop i =
    if i = stop then None
    else Some (i, i + 1) in
  Iter (start self, next (stop self))

include Iter.Input.Make0(struct
    type nonrec t = t
    type item = int
    let iter = iter
  end)

