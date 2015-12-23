
type t = float

let compare (a : t) (b : t) =
  Pervasives.compare a b

let to_string f = string_of_float f
let of_string s = float_of_string s

let abs = abs_float

let of_int = float_of_int
let to_int = int_of_float

let nan = Pervasives.nan
let infinity = Pervasives.infinity

