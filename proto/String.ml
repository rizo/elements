open Kernel
open Collection

type t = string
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


include Iterable0.With_Indexed(struct
    type t = string
    type index = int
    type item = char

    let length = length
    let unsafe_get i self = Stdlib.String.unsafe_get self i
  end)


(* Collection instance *)
include Collection0.Make(struct
    type nonrec t = t
    type nonrec item = item
    type accumulator = Stdlib.Buffer.t

    (* This is probably a bad idea. A new buffer needs to be created each time. *)
    let init = Buffer.create 16

    let reduce a acc = Buffer.add_char acc a; acc

    let extract acc =
      let s = Buffer.to_bytes acc in
      (* FIXME: A new buffer should be produced. This is not thread-safe. *)
      Buffer.reset acc;
      s
  end)


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

