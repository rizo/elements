
module type Forward = sig
  type a
  type s
  val next : s -> (a * s) option
end

module Fibonacci = struct
  type a = int
  type s = int * int
  let next (n, m) = Some (n, (m, n + m))
end

module Negative(Iter : Forward with type a = int) = struct
  type a = Iter.a
  type s = Iter.s

  let next s =
    match Iter.next s with
    | Some (a, s') -> Some (-a, s')
    | None -> None
end

module Map(Iter : Forward) = struct
  type a
  type s = Iter.s * (Iter.a -> a)

  let next (s, f) =
    match Iter.next s with
    | Some (a, s') -> Some (f a, (s', f))
    | None -> None
end

module Range = struct
  type a = int
  type s = int * int

  let next (n, m) =
    if n = m then None
    else Some (n, (n + 1, m))
end

module Folds (Iter : Forward) = struct
  let rec fold f z s =
    match Iter.next s with
    | Some (a, s') -> fold f (f z a) s'
    | None -> z
end

let range n m : Range.s = (n, m)

let fibonacci () : Fibonacci.s = (0, 1)

let negative (module Iter : Forward with type a = int) =
  let module Neg = Negative(Iter) in
  (module Neg : Forward)


