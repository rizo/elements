open Kernel
open Control

(** {1:opt Optional type and operations}

    Option represents an optional value: an [option] can be either [Some] and
    contain a value or a None, and do not. *)

type 'a t = 'a option
(** The type of optional values ['a]. *)

exception No_value
(** Exception raised when forcing a [None] option value. *)

val some : 'a -> 'a option
(** [some x] wraps [x] in a [Some] option value. *)

val none : 'a option
(** [none] is the [None] option value. *)

val is_some : 'a option -> bool
(** [is_some opt] is [true] if the option is a [Some] value. *)

val is_none : 'a option -> bool
(** [is_none opt] is [true] if the option is a [None] value. *)

val option : ('a -> 'b) -> (unit -> 'b) -> 'a option -> 'b
(** [option f default opt] is [f] called withe the [Some] value of [opt],
    or [default ()] if [opt] is [None].

    {[
      assert (None    |> option (fun () -> 0) ((+) 1) = 0);
      assert (Some 42 |> option (fun () -> 0) ((+) 1) = 43);
    ]} *)

val is_empty : 'a t -> bool
(** [is_empty opt] is [true] if the option is [None] and [false] otherwise. *)

val ( or ) : 'a option -> 'a -> 'a
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

val or_else : (unit -> 'a) -> 'a option -> 'a
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

val ( |> ) : 'a option -> ('a -> 'b) -> 'b option
(** [opt |> f] will apply [f] to the value wrapped by [opt], returning an
    option with the resulting value, or [None] if [opt] does not not have any
    value. This operator is an infix version of [map].

    Examples:

    {[
      assert (Some [1; 2; 3] |> List.reverse = Some [3; 2; 1]);
      assert (None |> Int.to_string = None);
    ]} *)

val force : 'a option -> 'a
(** [force opt] forces the extraction the optional value.

    @raise No_value if [opt] is [None].

    {[
      assert (Option.force (List.head [1; 2; 3]) = 1);
      assert (raises (Option.force (List.head [])))
    ]} *)

val catch : (unit -> 'a) -> 'a option
(** [catch f] wraps the call to [f], returning [None] if it raises an
    exception or the result as a [Some] value otherwise.

    {[
      Option.catch read_line or "nothing"
    ]} *)

val each : ('a -> unit) -> 'a option -> unit
(** [each f self] apply an effectful function [f] to the value wrapped in
    [self] or do nothing if [self] contains no value.

    {[
      Option.each print (Some "hey");
      Option.each print None
    ]} *)

(** {6 Implemented instances} *)
include Comparable1 with type 'a t := 'a t
include Default1    with type 'a t := 'a t
include Equatable1  with type 'a t := 'a t
include Hashable1   with type 'a t := 'a t
include Printable1  with type 'a t := 'a t
include Monad       with type 'a t := 'a t
include Functor     with type 'a t := 'a t

