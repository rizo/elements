
open Base

module String = Data_string

include Char

let of_str str =
  assert (String.len str = 1);
  String.get str 0

