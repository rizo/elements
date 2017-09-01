
(* Tuples *)

type ('a, 'b) pair = 'a * 'b

module Tuple2 = struct
  type ('a, 'b) t = ('a, 'b) pair

  let init a b = (a, b)

  let _1 (a, _) = a
  let _2 (_, b) = b
end


module Tuple3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let init a b c = (a, b, c)

  let _1 (a, _, _) = a
  let _2 (_, b, _) = b
  let _3 (_, _, c) = c
end


module Tuple4 = struct
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

  let init a b c d = (a, b, c, d)

  let _1 (a, _, _, _) = a
  let _2 (_, b, _, _) = b
  let _3 (_, _, c, _) = c
  let _4 (_, _, _, d) = d
end


module Pair = Tuple2

let _1  = Pair._1
let _2 = Pair._2

