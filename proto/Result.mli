open Kernel
open Control

(** {1:result Result type and operations} *)

type ('a, 'e) t = ('a, 'e) result
(** A result is either [Ok] meaning the computation succeeded, or it is an
    [Error] meaning that there was some failure. *)

exception Not_ok
(** Exception raised when forcing an [Error] result value. *)

val ok : 'a -> ('a, 'e) result
(** [ok x] creates an [Ok] result with value [x]. *)

val error : 'e -> ('a, 'e) result
(** [error x] creates an [Error] result with value [x]. *)

val is_ok : ('a, 'e) result -> bool
(** [is_ok res] is [true] if the result value is an [Ok]. *)

val is_error : ('a, 'e) result -> bool
(** [is_ok res] is [true] if the result value is an [Error]. *)

val force : ('a, 'e) result -> 'a
(** [force res] forces the extraction of the [Ok] value.

    @raise Not_ok if [res] is [Error].

    {[
      assert (Result.force (Ok 1) = 1);
      assert (raises (Result.force (Error "Oh no!")))
    ]} *)

val or_else : (unit -> 'a) -> ('a, 'e) result -> 'a
(** [or_else default res] extracts the [Ok] value of [res]. If the result is
    an [Error], the default value [default ()] is returned. A thunk is used
    instead of a direct value to avoid the default value evaluation when the
    result is not [Ok].

    Examples:

    {[
      assert (Error "no" |> Result.or_else (fun () -> 42) = 42);

      (* Note: [read_line] is not called in the following example. *)
      assert (Ok "Bob" |> Result.or_else read_line = "Bob");
    ]} *)

val ( or ) : ('a, 'e) result -> 'a -> 'a
(** [res or x] is the flipped infix version of [with_default] equivalent to
    [with_default (fun () -> x) res].

    {[
      assert ((Error "something bad" or "it's fine") = "it's fine");
      assert ((Error "something bad" or Ok 42 or 0) = 42);
      assert ((Error "something bad" or Error "really bad" or 0) = 0);
      assert ((Error "something bad" or 1 + 2) = 3);
    ]}

    Note: [or] has a very low precedence, expressions have to be grouped to
    delimit the scope. *)

val or_fail : string -> ('a, 'e) result -> 'a
(** [or_fail message self] forces the extraction of the [Ok] value and
    {!fail}s if [self] is [Error].

    @raise Failure if [self] is [Error].

    {[
      assert (Ok 42 |> Option.or_fail "no value" = 1);
    ]} *)

val ( <@> ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
(** [f <@> self] will apply [f] to the [Ok] value wrapped by [self], returning
    [Ok] with the resulting value, or [Error] if [self] does not not have any
    value. This operator is an infix version of [map].

    Examples:

    {[
      assert (List.reverse <@> Ok [1; 2; 3] == Ok [3; 2; 1]);
      assert (Int.to_string <@> Error "nothing" == Error "nothing");
    ]} *)

val catch : (unit -> 'a) -> ('a, exn) result
(** [catch f] wraps the call to [f], returning [None] if it raises an
    exception or the result as a [Some] value otherwise.

    {[
      match Result.catch (fun () -> 1 / 0) with
      | Ok x    -> print "All ok."
      | Error e -> print (format "I saved you from %s!" (Exn.show e))
    ]} *)

val case : ('a -> 'b) -> ('e -> 'b) -> ('a, 'e) result -> 'b
(** [case f default res] is the application of f to the [Ok] value of [res],
    or to [default e] value if the [res] is [Error e]. *)

val to_option : ('a, 'e) result -> 'a option
(** [to_option res] is the result value represented as option. [Ok x] becomes
    [Some x] and [Error e] becomes [None].

    {[
      assert (to_option (Ok 42)      = Some 42);
      assert (to_option (Error "no") = None);
    ]}*)

val of_option : 'a option -> 'e -> ('a, 'e) result
(** [of_option opt err] is a result created with optional value [opt]. [Some x]
    becomes [Ok x] and [Error e] becomes [err].

    {[
      assert (of_option Some 42      = Ok 42);
      assert (of_option None "oh no" = Error "oh no");
    ]}*)

(** {6 Implemented instances} *)
include Comparable2 with type ('a, 'e) t := ('a, 'e) t
include Equatable2  with type ('a, 'e) t := ('a, 'e) t
include Hashable2   with type ('a, 'e) t := ('a, 'e) t
include Printable2  with type ('a, 'e) t := ('a, 'e) t
include Monad2      with type ('a, 'e) t := ('a, 'e) t
include Functor2    with type ('a, 'e) t := ('a, 'e) t

