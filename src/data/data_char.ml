
module String = Data_string

open Base

include Char


let of_string str =
  assert (String.length str = 1);
  String.get str 0

