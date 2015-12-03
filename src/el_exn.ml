
open El_base

let guard f x =
  try Ok (f x)
  with e -> Error e

let fail msg = raise (Failure msg)

let to_string = Printexc.to_string

