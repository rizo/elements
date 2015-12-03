
type t = int

let cmp (a: int) (b: int) = Pervasives.compare a b

let to_str i = string_of_int i
let of_str s = int_of_string s

let abs = Pervasives.abs


