
open Base

type range = Range of (int * int)

let init n m = Range (n, m)

let start (Range (n, _)) = n
let stop  (Range (_, m)) = m


