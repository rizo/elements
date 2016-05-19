
type t
(** Standard integer type. *)

val compare : t -> t -> int
(** Compares to integer values. *)

val pp : Format.formatter -> t -> unit
(** Pretty print integer values. *)

external format : string -> int -> string = "caml_format_int"

external read : string -> int = "caml_int_of_string"
(** Parse integer from string representation. *)

val show : int -> string
(** Convert integer to string representation. *)

val abs : t -> t
(** Absolute integer value. *)

