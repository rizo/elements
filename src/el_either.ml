
open El_base

type ('a, 'b) t = ('a, 'b) either

let return x = Right x

let (>>=) m f =
  match m with
  | Right x -> f x
  | Left e  -> Left e

