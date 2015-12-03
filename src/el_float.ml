
type t = float

let cmp (a: float) (b: float) = Pervasives.compare a b

let to_str f = string_of_float f
let of_str s = float_of_string s

let abs = abs_float

