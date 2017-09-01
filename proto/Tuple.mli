
(** {1:tup Tuples} *)

type ('a, 'b) pair = 'a * 'b

module Tuple2 : sig
  type ('a, 'b) t = ('a, 'b) pair

  val init : 'a -> 'b -> ('a, 'b) t
  (** [init a b] is [(a, b)], i.e., a tuple with two elements. *)

  val _1 : ('a, 'b) t -> 'a
  val _2 : ('a, 'b) t -> 'b
  (** [_1] and [_2] are functions to get the first and second elements of the
      tuple respectively.

      {[
        assert (_1 (1, "x", [1; 2], true) = 1);
        assert (_2 (1, "x", [1; 2], true) = "x");
      ]} *)
end

module Tuple3 : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  val init : 'a -> 'b -> 'c -> ('a, 'b, 'c) t
  (** [init a b c] is [(a, b, c)], i.e., a tuple with three elements. *)

  val _1  : ('a, 'b, 'c) t -> 'a
  val _2  : ('a, 'b, 'c) t -> 'b
  val _3  : ('a, 'b, 'c) t -> 'c
  (** [_1], [_2] and [_3] are functions to get the first, second
      and third elements of the tuple respectively.

      {[
        assert (_1 (1, "x", [1; 2], true) = 1);
        assert (_2 (1, "x", [1; 2], true) = "x");
        assert (_3 (1, "x", [1; 2], true) = [1; 2]);
      ]} *)
end

module Tuple4 : sig
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

  val init : 'a -> 'b -> 'c -> 'd -> ('a, 'b, 'c, 'd) t
  (** [init a b c d] is [(a, b, c, d)], i.e., a tuple with four elements. *)

  val _1  : ('a, 'b, 'c, 'd) t -> 'a
  val _2  : ('a, 'b, 'c, 'd) t -> 'b
  val _3  : ('a, 'b, 'c, 'd) t -> 'c
  val _4  : ('a, 'b, 'c, 'd) t -> 'd
  (** [_1], [_2], [_3] and [_4] are functions to get the first, second, third
      and fourth elements of the tuple respectively.

      {[
        assert (_1 (1, "x", [1; 2], true) = 1);
        assert (_2 (1, "x", [1; 2], true) = "x");
        assert (_3 (1, "x", [1; 2], true) = [1; 2]);
        assert (_4 (1, "x", [1; 2], true) = true);
      ]} *)
end

module Pair : module type of Tuple2
(** [Pair] is an alias for [Tuple2]. *)

val _1  : ('a, 'b) Pair.t -> 'a
(** [_1 pair] is the first elements of the [pair]

    {[
      assert (_1 (1, "x") = 1)
    ]} *)

val _2 : ('a, 'b) Pair.t -> 'b
(** [_2 pair] is the second elements of the [pair]

    {[
      assert (_2 (1, "x") = "x")
    ]} *)

