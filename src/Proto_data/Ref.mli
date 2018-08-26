open Local
open Control

type 'a t = 'a Stdlib.ref

val make : 'a -> 'a t
(** [make x] is a reference that contains the [x] value.

    This functions is globally exported as {!ref}.

    {[
      assert (Ref.(get (make 42)) == 42)
    ]} *)

val ( ! ) : 'a t -> 'a
val get : 'a t -> 'a

val ( := ) : 'a t -> 'a -> unit
val set : 'a t -> 'a -> unit

val swap : 'a t -> 'a t -> unit

val modify : ('a -> 'a) -> 'a t -> unit


include Functor with type 'a t := 'a t
