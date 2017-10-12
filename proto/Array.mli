open Collection

type 'a t = 'a array

val make : int -> (int -> 'a) -> 'a t

val length : 'a t -> int

val append : 'a t -> 'a t -> 'a t

val foldk : ('a -> 'r -> ('r -> 'r) -> 'r) -> 'r -> 'a t -> 'r
val inspect : int -> ('a -> 'r) -> 'r -> 'a t -> 'r

include Iterable  with type 'a t := 'a t
include Container with type 'a t := 'a t

module Unsafe : sig
  val get : int -> 'a array -> 'a
end

