open Kernel
open Collection

type t = string
type item = char

let length = Stdlib.String.length

let split ?on:(sep = ' ') self =
  let n = Stdlib.String.length self in
  if n = 0 then Iter.empty else
  let next f r i =
    if i > n then r else
    let j =
      try Stdlib.String.index_from self i sep
      with Not_found -> n in
    let s = Stdlib.String.sub self i (j - i) in
    f s (j + 1) in
  Iter { init = (fun () -> 0); next; stop = ignore }

let join ?on:(sep = "") iter =
  Iter.reduce (fun a b -> a ^ sep ^ b) iter or ""


include Iterable0.With_Indexed(struct
    type t = string
    type index = int
    type item = char

    let length = length
    let unsafe_get i self = Stdlib.String.unsafe_get self i
  end)


(* Collection instance *)
include Collectable0.Make(struct
    type nonrec t = t
    type nonrec item = item
    type accumulator = Stdlib.Buffer.t

    let empty = ""

    let accumulator self =
      let acc = Stdlib.Buffer.create 16 in
      if self <> "" then begin
        Stdlib.Buffer.add_string acc self
      end;
      acc

    let reduce a acc = Stdlib.Buffer.add_char acc a; acc

    let extract acc =
      Stdlib.Bytes.to_string (Stdlib.Buffer.to_bytes acc)
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

