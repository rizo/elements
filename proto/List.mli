open Kernel
open Control
open Collection

type 'a t = 'a list
(** List type with items of type ['a]. *)

(* XXX: https://github.com/ocaml/ocaml/compare/trunk...Richard-Degenne:list-init?expand=1#diff-59bc62b6e16ab262d703d043e2d1d51b *)
(* https://github.com/ocaml/ocaml/pull/1034 *)
(* I believe that calling the function left-to-right, from 0 to n - 1, is an
   important property of a no-surprise implementation. *)
val make : int -> (int -> 'a) -> 'a t
(** [init n f] is a list of length [n] with each item produced by applying [f]
    to its index.

    If [n] is negative a reversed list will be produced with absolute indices.

    {[
      assert (make 5    identity = [0; 1; 2; 3; 4]);
      assert (make (-5) identity = [4; 3; 2; 1; 0]);
    ]} *)

(* To be included as the [Collection] class. *)
val add : 'a -> 'a t -> 'a t

val tail : 'a t -> 'a list option

val reverse : 'a t -> 'a t

val indexed : ?from: int -> 'a t -> (int * 'a) t

val concat : 'a t t -> 'a t

val chunks : int -> 'a t -> 'a t t

val of_array : 'a array -> 'a t

val inspect : ('a -> 'a t -> 'r) -> 'r -> 'a t -> 'r

val foldk : ('a -> 'r -> ('r -> 'r) -> 'r) -> 'r -> 'a t -> 'r

val take : int -> 'a t -> 'a t


module Unsafe : sig
  val head : 'a t -> 'a
  (** [Unsafe.head] is the unsafe version of [head].

      @raise [Failure "List.Unsafe.head"] if the list is empty. *)

  val tail : 'a t -> 'a t
  (** [Unsafe.tail] is the unsafe version of [tail].

      @raise [Failure "List.Unsafe.tail"] if the list is empty. *)

  val get : int -> 'a t -> 'a
  (** [Unsafe.get i l] is the unsafe version of [get].

      @raise [Failure "List.Unsafe.get"] if the list is too short.
      @raise [Invalid_argument "List.Unsafe.get"] if [i] is negative. *)
end


(** {6 Implemented instances} *)
include Functor     with type 'a t := 'a t
include Iterable    with type 'a t := 'a t
include Collection  with type 'a t := 'a t
include Monoid      with type 'a t := 'a t
include Default1    with type 'a t := 'a t
include Comparable1 with type 'a t := 'a t

