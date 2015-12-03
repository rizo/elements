
type t = int

let compare (a: int) (b: int) = Pervasives.compare a b

let to_string i = string_of_int i
let of_string s = int_of_string s

let abs = Pervasives.abs


