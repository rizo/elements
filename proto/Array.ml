open Kernel
open Control

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


(* Container instance *)
include Collection.Container.With_indexable(struct
    type nonrec 'a t = 'a t
    type index = int

    let length = Stdlib.Array.length
    let unsafe_get i self = Stdlib.Array.unsafe_get self i
  end)


(* Iterable instance *)
include Collection.Iterable.Make(struct
    type 'a t = 'a array
    type 'a state = int

    let init a = 0

    let next self state f r =
      if state = length self then r
      else f (Stdlib.Array.unsafe_get self state) (state + 1)
  end)


let make = Stdlib.Array.init

let uncheked_get i self =
  Stdlib.Array.unsafe_get self i

let length = Stdlib.Array.length

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


