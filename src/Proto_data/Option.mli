open Kernel
open Control

(** {1:opt Optional type and operations}

    Option represents an optional value: an [option] can be either [Some] and
    contain a value or a None, and do not. *)

type 'a t = 'a option
(** The type of optional values ['a]. *)

exception No_value
(** Exception raised when forcing a [None] option value. *)

val some : 'a -> 'a t
(** [some x] wraps [x] in a [Some] option value. *)

val none : 'a t
(** [none] is the [None] option value. *)

val is_some : 'a t -> bool
(** [is_some opt] is [true] if the option is a [Some] value. *)

val is_none : 'a t -> bool
(** [is_none opt] is [true] if the option is a [None] value. *)

val case : (unit -> 'b) -> ('a -> 'b) -> 'a t -> 'b
(** [case default f self] is [f] applied to the [Some] value of [self],
    or [default ()] if [self] is [None].

    {[
      assert (None    |> Option.case (fun () -> 0) ((+) 1) = 0);
      assert (Some 42 |> Option.case (fun () -> 0) ((+) 1) = 43);
    ]} *)

val is_empty : 'a t -> bool
(** [is_empty opt] is [true] if the option is [None] and [false] otherwise. *)

val ( or ) : 'a t -> 'a -> 'a
(** [opt or x] is the flipped infix version of [with_default] equivalent to
    [with_default (fun () -> x) opt].

    Examples:

    {[
      assert ((List.head [] or 0) = 0);
      assert ((List.head [] or List.head [1] or 0) = 1);
      assert ((List.head [] or List.head []  or 0) = 0);
      assert ((List.head [] or 1 + 2) = 3);
    ]}

    Note: [or] has a very low precedence, expressions have to be grouped to
    delimit the scope. *)

val or_else : (unit -> 'a) -> 'a t -> 'a
(** [or_else f opt] extracts the optional value. If the optional is
    [None], the default value [f ()] is returned. A thunk is used instead of
    a direct value to avoid the default value evaluation when the option is
    set.

    Examples:

    {[
      assert (List.head [1; 2; 3] |> Option.or_else (fun () -> 0) = 1);

      (* [read_line] is not called in the following example. *)
      assert (Some "Bob" |> Option.with_default read_line = "Bob");
    ]} *)

val or_fail : string -> 'a t -> 'a
(** [or_fail message opt] forces the extraction of the optional value and
    {!fail}s if [self] does not contain a value.

    @raise Failure if [self] is [None].

    {[
      assert (List.head [1; 2; 3] |> Option.or_fail "empty list" = 1);
    ]} *)

(* XXX: Better docs *)
(* val ( <@> ) : 'a t -> ('a -> 'b) -> 'b t *)
(** [f <@> self] will apply [f] to the value wrapped by [self], returning an
    option with the resulting value, or [None] if [self] does not not have any
    value. This operator is an infix version of [map].

    Examples:

    {[
      assert (List.reverse <@> Some [1; 2; 3] == Some [3; 2; 1]);
      assert (Int.to_string <@> None == None);
    ]} *)

val force : 'a t -> 'a
(** [force opt] forces the extraction the optional value.

    @raise No_value if [opt] is [None].

    {[
      assert (Option.force (List.head [1; 2; 3]) = 1);
      assert (raises (Option.force (List.head [])))
    ]} *)

val catch : (unit -> 'a) -> 'a t
(** [catch f] wraps the call to [f], returning [None] if it raises an
    exception or the result as a [Some] value otherwise.

    {[
      Option.catch read_line or "nothing"
    ]} *)

val apply : ('a -> unit) -> 'a t -> unit
(** [apply f self] applies an effectful function [f] to the value wrapped in
    [self] or does nothing if [self] contains no value.

    {[
      Option.apply print (Some "hey");
      Option.apply print None
    ]} *)

val to_bool : 'a t -> bool
(** [to_bool self] is an alis for {!is_some}.

    {[
      assert (Option.to_bool (Some 42) == true);
      assert (Option.to_bool None == false);
    ]} *)

val to_list : 'a t -> 'a list
(** [to_list self] is a singleton list with the value wrapped by [self] or an empty list if [self] is [None].

  {[
    assert (Option.to_bool (Some 42) == true);
    assert (Option.to_bool None == false);
  ]} *)


(** {6 Implemented instances} *)
include Comparable1 with type 'a t := 'a t
include Default1    with type 'a t := 'a t
include Equatable1  with type 'a t := 'a t
include Hashable1   with type 'a t := 'a t
include Printable1  with type 'a t := 'a t
include Monad       with type 'a t := 'a t
include Functor     with type 'a t := 'a t

