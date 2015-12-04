
open El_base

include Printexc

let guard f x =
  try Ok (f x)
  with e -> Error e

let fail msg = raise (Failure msg)


