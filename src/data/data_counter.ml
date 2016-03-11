
module Map = Data_map

open Base

module type S = sig
  include Map.S
  val incr : ?by: int -> key: key -> int t -> int t
  val decr : ?by: int -> key: key -> int t -> int t
  val reset : key: key -> int t -> int t
end

module Make(K : Map.OrderedType) : S
  with type 'a t = 'a Map.Make(K).t
   and type key = K.t =
struct
  include Map.Make(K)

  let incr ?(by = 1) ~key m =
    let n = find_def key 1 m in
    if n + by <= 0
    then remove key m
    else add m ~key ~data:(n + by)

  let decr ?(by = 1) ~key m =
    let n = find_def key (- 1) m in
    if n - by <= 0
    then remove key m
    else add m ~key ~data:(n - by)

  let reset ~key m =
    remove key m
end

