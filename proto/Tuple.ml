
(* Tuples *)

type ('a, 'b) pair = 'a * 'b

module Tuple2 = struct
  type ('a, 'b) t = ('a, 'b) pair

  let init a b = (a, b)

  let first (a, _) = a
  let second (_, b) = b
end


module Tuple3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let init a b c = (a, b, c)

  let first (a, _, _) = a
  let second (_, b, _) = b
  let third (_, _, c) = c
end


module Tuple4 = struct
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

  let init a b c d = (a, b, c, d)

  let first (a, _, _, _) = a
  let second (_, b, _, _) = b
  let third (_, _, c, _) = c
  let fourth (_, _, _, d) = d
end


module Pair = Tuple2

let first = Kernel.first
let second = Kernel.second

