open Kernel
open Control
open Collection

type 'a t = 'a array

val make : int -> (int -> 'a) -> 'a t

val length : 'a t -> int

val foldk : ('a -> 'r -> ('r -> 'r) -> 'r) -> 'r -> 'a t -> 'r

val inspect : int -> ('a -> 'r) -> 'r -> 'a t -> 'r

module Unsafe : sig
  val get : int -> 'a array -> 'a
end


include Monoid    with type 'a t := 'a t
include Default1  with type 'a t := 'a t
include Functor   with type 'a t := 'a t
include Iterable  with type 'a t := 'a t
include Container with type 'a t := 'a t

