open Kernel

module Stdlib = Proto_shadow_stdlib

type 'a t = 'a array

let make = Stdlib.Array.init

let uncheked_get i self =
  Stdlib.Array.unsafe_get self i

let length = Stdlib.Array.length

let append = Stdlib.Array.append

let foldk f init self =
  let n = length self in
  let rec go i r =
    if Int.(i == n) then r
    else f (uncheked_get i self) r (fun r' -> go (i + 1) r') in
  go 0 init


let inspect index f r self =
  if equal index (length self) then r
  else f (uncheked_get index self)


module Indexable_base = struct
  type nonrec 'a t = 'a t
  type index = int

  let length = length
  let unsafe_get i self = Stdlib.Array.unsafe_get self i
end

include Collection.Container.With_indexable(Indexable_base)

module Iterable_base = struct
  type 'a t = 'a array
  type 'a state = int

  let init a = 0

  let next f r state self =
    if state = length self then r
    else f (Stdlib.Array.unsafe_get self state) (state + 1)
end
include Collection.Iterable.Make(Iterable_base)


module Unsafe = struct
  let get i self =
    Stdlib.Array.get self i
end


