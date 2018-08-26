

type ('a, 'b) t = ('a, 'b) Kernel.either = Left of 'a | Right of 'b

let return x = Right x

let (>>=) m f =
  match m with
  | Right x -> f x
  | Left e  -> Left e

