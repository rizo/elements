
open Base

include Char

let of_str str =
  assert (Str.len str = 1);
  Str.get str 0

