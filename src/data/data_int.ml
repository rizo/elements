
type t = int

let compare (a : t) (b : t) = Pervasives.compare a b

let pp = Format.pp_print_int

external format : string -> int -> string = "caml_format_int"

external read : string -> int = "caml_int_of_string"

let show self = format "%d" self

let abs = Pervasives.abs

