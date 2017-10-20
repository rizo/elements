open Kernel
open Control
open Collection

type 'a t = 'a array


(* Monoid instance *)
include Monoid.Make(struct
    type nonrec 'a t = 'a t

    let empty = [||]
    let append = Stdlib.Array.append
  end)


(* Default instance *)
let default = empty


(* Functor instance *)
include Functor.Make(struct
    type nonrec 'a t = 'a t

    let map = Stdlib.Array.map
  end)


(* Iterable instance *)
include Iterable.With_Indexed(struct
    type nonrec 'a t = 'a t
    type index = int

    let length = Stdlib.Array.length
    let unsafe_get i self = Stdlib.Array.unsafe_get self i
  end)


(* Collection instance *)
include Collection.Make(struct
    type nonrec 'a t = 'a t
    type 'a accumulator = 'a list * int

    let init = ([], 0)

    let reduce a (acc, len) = (a :: acc, len + 1)

    let extract (acc, len) =
      match acc with
      | [] -> [||]
      | x :: l ->
        let a = Stdlib.Array.make len x in
        let rec loop i l =
          match l with
          | [] -> assert (i = -1)
          | x :: l ->
              Stdlib.Array.set a i x;
              loop (i - 1) l in
        loop (len - 2) l;
        a
  end)


let make = Stdlib.Array.init

let uncheked_get i self =
  Stdlib.Array.unsafe_get self i

let length = Stdlib.Array.length

let of_list = Stdlib.Array.of_list

let foldk f init self =
  let n = length self in
  let rec go i r =
    if Int.(i == n) then r
    else f (uncheked_get i self) r (fun r' -> go (i + 1) r') in
  go 0 init


let inspect index f r self =
  if equal index (length self) then r
  else f (uncheked_get index self)


module Unsafe = struct
  let get i self =
    Stdlib.Array.get self i
end


