
type t = float

let compare (a: float) (b: float) = Pervasives.compare a b

let to_string f = string_of_float f
let of_string s = float_of_string s

let abs = abs_float

