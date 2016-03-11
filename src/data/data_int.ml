
type t = int
  [@@deriving show]

let compare (a : t) (b : t) = Pervasives.compare a b

let to_string i = string_of_int i
let of_string s = int_of_string s

let abs = Pervasives.abs

