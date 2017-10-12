module Stdlib = Proto_shadow_stdlib
open Kernel

(* include Caml.String *)

type t = Stdlib.String.t
type item = char

let split ?(on=' ') str =
  let rec indices acc i =
    try
      let i = succ (Stdlib.String.index_from str i on) in
      indices (i::acc) i
    with Not_found ->
      (Stdlib.String.length str + 1) :: acc
  in
  let is = indices [0] 0 in
  let rec aux acc = function
    | last::start::tl ->
      let w = Stdlib.String.sub str start (last - start - 1) in
      aux (w::acc) (start::tl)
    | _ -> acc
  in
  aux [] is

let length = Stdlib.String.length

let inspect index f r self =
  if equal index (length self) then r
  else f (Stdlib.String.unsafe_get self index)


module Indexable_base = struct
  type t = string
  type index = int
  type item = char

  let length = length
  let unsafe_get i self = Stdlib.String.unsafe_get self i
end

include Collection.Container0.With_indexable(Indexable_base)

module Iterable_base = struct
  type t = string
  type state = int
  type item = char

  let init a = 0

  let next f r state self =
    if state = Stdlib.String.length self then r
    else f (Stdlib.String.unsafe_get self state) (state + 1)
end
include Collection.Iterable0.Make(Iterable_base)

(* Printable instance *)
include Printable.Make(struct
  type nonrec t = t
  let pp = Format.pp_print_string
end)


(* Parsable instance *)
let parse x = Some x (* FIXME *)

(* Hashable instance *)
let hash x = Hashtbl.hash x

(* Comparable instance *)
include Comparable.Make(struct
  type nonrec t = t
  let compare (s1 : string) (s2 : string) =
    Pervasives.compare s1 s2
end)

(* Default instance *)
let default = ""

