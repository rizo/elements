open Kernel
open Control

module type Set = sig
  type t
  type item

  (* Collectable *)
  val add : item -> t -> t
  val singleton : item -> t

  val is_empty : t -> bool
  val contains : item -> t -> bool
  val length : t -> int

  val remove : item -> t -> t
  val remove_index : int -> item -> t -> t

  val union : t -> t -> t

  val intersection : t -> t -> t

  val difference : t -> t -> t
  (** [difference s1 s2] is the difference of two sets as a new set. *)

  val symmetric_difference : t -> t -> t
  (** [symmetric_difference s1 s2] is the symmetric difference of two sets as a
      new set. *)

  val is_subset : t -> of': t -> bool
  (** [is_subset self ~of':other] is [true] iff [self1] is a subset of [other].

      @see {!is_superset}

      Examples:

      {[
        let s1 = Int.Set.make 3 identity in
        let s2 = Int.Set.make 5 identity in
        assert (s1 |> is_subset ~of':s2);
        assert (not (s2 |> is_subset ~of':s1))
      ]} *)

  val is_superset : t -> of': t -> bool

  val each : (item -> unit) -> t -> unit
  val filter : (item -> bool) -> t -> t

  val partition : (item -> bool) -> t -> t * t
  val split : at: item -> t -> t * bool * t

  val group_by : (item -> item -> bool) -> t -> t list

  val fold : (item -> 'a -> 'a) -> t -> 'a -> 'a
  val all : (item -> bool) -> t -> bool
  val any : (item -> bool) -> t -> bool

  val find_min : t -> item option
  val find_max : t -> item option

  val choose : t -> item option
  (** [choose self] is an arbitrary element from the set [self], or [None] if
      the set is empty. *)

  val get : item -> t -> item option
  val find_first : (item -> bool) -> t -> item option
  val find_last : (item -> bool) -> t -> item option

  val of_list : item list -> t
  val to_list : t -> item list

  val of_array : item array -> t
  val to_array : t -> item array

  include Comparable with type t := t
  include Equatable  with type t := t
  include Monoid0    with type t := t
  include Default    with type t := t
  include Functor0   with type t := t and type item := item
end

