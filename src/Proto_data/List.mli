open Kernel
open Control
open Collection

type 'a t = 'a list
(** Singly-linked list type with items of type ['a]. *)


(** {6 Implemented Instances} *)

include Functor     with type 'a t := 'a t
include Iterable    with type 'a t := 'a t
include Collectable with type 'a t := 'a t
include Monoid      with type 'a t := 'a t
include Default1    with type 'a t := 'a t
include Comparable1 with type 'a t := 'a t


(* {6 List operations} *)

val case : ('a -> 'a t -> 'r) -> 'r -> 'a t -> 'r
(** [case f empty self] deconstructs the list [self] into its
    internal representation cases, applying [f] to the first item and the rest
    of the list, or returning [empty] if the list is empty.

    This function is equivalent to pattern-matching on lists but may lead to
    more composable and efficient code.

    Time complexity: O(1)

    @see <https://#TODO> Iteration in Proto
    @see <https://en.wikipedia.org/wiki/Mogensen–Scott_encoding> Mogensen–Scott encoding

    {[
      (* Get the first element of the list. *)
      assert ([1; 2; 3] |> List.case (fun a rest -> Some a) None = Some 1);

      (* Check if the list is empty. *)
      assert ([] |> List.case (fun a rest -> false) true = true);
    ]} *)

val add : 'a -> 'a t -> 'a t
(** [add item self] adds a given [item] to the front of the list [self]. This
    function is also known as [cons]truct and is equivalent to the [::]
    operator.

    Time complexity: O(1)

    {[
      assert (List.add 'x' [] = ['x']);
      assert (List.add 100 [101; 102; 103] = [100; 101; 102; 103]);
    ]} *)

val first : 'a t -> 'a option
(** [first self] is the first item in list [self] or [None] is the list is empty.

    Time complexity: O(1)

    {[
      assert (List.first [] = None);
      assert (List.first (List.add 42 []) = Some 42);
      assert (List.first [1; 2; 3; 4] = Some 1);
    ]} *)

val rest : 'a t -> 'a list option
(** [rest self] is the list [self] without the first element. If the list is
    empty [None] is returned. This function is also known as [head].

    Time complexity: O(1)

    {[
      assert (List.rest [] = None);
      assert (List.rest (List.add 42 []) = Some 42);
      assert (List.rest [1; 2; 3; 4] = Some [2; 3; 4]);
    ]} *)

val append : 'a t -> 'a t -> 'a t
(** [append self other] adds all items from the list [other] to the end of
    [self].

    Time complexity: O(2 * length(other))

    {[
      assert (List.append [] [] = []);
      assert (List.append [1; 2; 3] [] = [1; 2; 3]);
      assert (List.append [] [1; 2; 3] = [1; 2; 3]);
      assert (List.append [1; 2; 3] [4; 5; 6] = [1; 2; 3; 4; 5; 6]);
    ]} *)

val reverse : 'a t -> 'a t
(** [reveres self] produces a list by reversing the order of items in [self].

    Time complexity: O(length(self))

    {[
      assert (List.reverse [] = []);
      assert (List.reverse [1; 2; 3; 4] = [4; 3; 2; 1]);
      assert (List.reverse [1; 2; 3; 4] = List.foldl List.add [] [1; 2; 3; 4]);
    ]} *)


(** {6 Conversions} *)

val to_array : 'a t -> 'a array
(** [to_array self] is an array with all items from [self]. *)

val of_array : 'a array -> 'a t
(** [of_array array] is a list with all items from [array]. *)

val to_list : 'a list -> 'a t
(** [to_list] is the identity function. It exists to provide consistency for
    module-level programming. *)

val of_list : 'a list -> 'a t
(** [of_list] is the identity function. It exists to provide consistency for
    module-level programming. *)


(** {6 Unsafe Operations} *)

module Unsafe : sig
  val first : 'a t -> 'a
  (** [Unsafe.first] is the unsafe version of {!List.first}.

      @raise Failure if the list is empty. *)

  val rest : 'a t -> 'a t
  (** [Unsafe.rest] is the unsafe version of {!List.rest}.

      @raise Failure if the list is empty. *)

  val get : int -> 'a t -> 'a
  (** [Unsafe.get i self] is the unsafe version of {!List.get}.

      @raise Failure ["List.Unsafe.get"] if the list [self] is too short.
      @raise Invalid_argument ["List.Unsafe.get"] if [i] is negative. *)
end

