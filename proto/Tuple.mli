
(** {1:tup Tuples} *)

type ('a, 'b) pair = 'a * 'b

module Tuple2 : sig
  type ('a, 'b) t = ('a, 'b) pair

  val init : 'a -> 'b -> ('a, 'b) t
  (** [init a b] is [(a, b)], i.e., a tuple with two elements. *)

  val first : ('a, 'b) t -> 'a
  val second : ('a, 'b) t -> 'b
  (** [first] and [second] are functions to get the first and second elements of the
      tuple respectively.

      {[
        assert (first (1, "x", [1; 2], true) = 1);
        assert (second (1, "x", [1; 2], true) = "x");
      ]} *)
end

module Tuple3 : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  val init : 'a -> 'b -> 'c -> ('a, 'b, 'c) t
  (** [init a b c] is [(a, b, c)], i.e., a tuple with three elements. *)

  val first : ('a, 'b, 'c) t -> 'a
  val second : ('a, 'b, 'c) t -> 'b
  val third : ('a, 'b, 'c) t -> 'c
  (** [first], [second] and [third] are functions to get the first, second
      and third elements of the tuple respectively.

      {[
        assert (first (1, "x", [1; 2], true) = 1);
        assert (second (1, "x", [1; 2], true) = "x");
        assert (third (1, "x", [1; 2], true) = [1; 2]);
      ]} *)
end

module Tuple4 : sig
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

  val init : 'a -> 'b -> 'c -> 'd -> ('a, 'b, 'c, 'd) t
  (** [init a b c d] is [(a, b, c, d)], i.e., a tuple with four elements. *)

  val first : ('a, 'b, 'c, 'd) t -> 'a
  val second : ('a, 'b, 'c, 'd) t -> 'b
  val third : ('a, 'b, 'c, 'd) t -> 'c
  val fourth : ('a, 'b, 'c, 'd) t -> 'd
  (** [first], [second], [third] and [fourth] are functions to get the first, second, third
      and fourth elements of the tuple respectively.

      {[
        assert (first (1, "x", [1; 2], true) = 1);
        assert (second (1, "x", [1; 2], true) = "x");
        assert (third (1, "x", [1; 2], true) = [1; 2]);
        assert (fourth (1, "x", [1; 2], true) = true);
      ]} *)
end

module Pair : module type of Tuple2
(** [Pair] is an alias for [Tuple2]. *)

val first : ('a, 'b) Pair.t -> 'a
(** [first pair] is the first elements of the [pair]

    {[
      assert (first (1, "x") = 1)
    ]} *)

val second : ('a, 'b) Pair.t -> 'b
(** [second pair] is the second elements of the [pair]

    {[
      assert (second (1, "x") = "x")
    ]} *)

