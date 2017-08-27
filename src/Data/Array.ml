
type 'a t = 'a array

external idx : 'a array -> int -> 'a = "%array_unsafe_get"
external len : 'a array -> int = "%array_length"
external get_exn : 'a array -> int -> 'a = "%array_safe_get"
external set: 'a array -> int -> 'a -> unit = "%array_safe_set"
external unsafe_get: 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set: 'a array -> int -> 'a -> unit = "%array_unsafe_set"
external make: int -> 'a -> 'a array = "caml_make_vect"
external unsafe_sub : 'a array -> int -> int -> 'a array = "caml_array_sub"
external append_prim : 'a array -> 'a array -> 'a array = "caml_array_append"
external concat : 'a array list -> 'a array = "caml_array_concat"
external unsafe_blit : 'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit"
external make_float: int -> float array = "caml_make_float_vect"

let init   = Stdlib.Array.init
let blit   = Stdlib.Array.blit
let copy   = Stdlib.Array.copy
let append = Stdlib.Array.append
let fill   = Stdlib.Array.fill
let sort   = Stdlib.Array.sort

let copy_and_add a x =
  let a_len = len a in
  let new_a = make (a_len + 1) x in
  blit a 0 new_a 0 a_len;
  new_a

