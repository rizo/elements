
module Data    = Data
module Control = Control

(** Export Base definitions. *)
include module type of Kernel

(** Export Data modules and definitions. *)
module Option = Data.Option
module Result = Data.Result

type ('a, 'b) pair = ('a, 'b) Data.Tuple.pair
(** Global alias for {!Data.Tuple.pair}. *)

val _1  : ('a, 'b) pair -> 'a
(** Global alias for {!Data.Tuple._1}. *)

val _2 : ('a, 'b) pair -> 'b
(** Global alias for {!Data.Tuple._2}. *)

type void = Data.Void.t
(** Global alias for {!Void.t}. *)


(** {6 Global character operations} *)

val char : int -> char option
(** Global alias for {!Char.option_of_int}. *)

val code : char -> int
(** Global alias for {!Char.to_int}. *)


(** {6 Global option operations} *)

val some : 'a -> 'a option
(** Global alias for {!Option.some}. *)

val none : 'a option
(** Global alias for {!Option.none}. *)

val is_some : 'a option -> bool
(** Global alias for {!Option.is_some}. *)

val is_none : 'a option -> bool
(** Global alias for {!Option.is_none}. *)

val option : ('a -> 'b) -> (unit -> 'b) -> 'a option -> 'b
(** Global alias for {!Option.option}. *)

val ( or ) : 'a option -> 'a -> 'a
(** Global alias for {!Option.( or )}. *)


(* (** {6 Global result operations} *) *)

val ok : 'a -> ('a, 'e) result
(** Global alias for {!Result.ok}. *)

val error : 'e -> ('a, 'e) result
(** Global alias for {!Result.error}. *)

val is_ok : ('a, 'e) result -> bool
(** Global alias for {!Result.is_ok}. *)

val is_error : ('a, 'e) result -> bool
(** Global alias for {!Result.is_error}. *)

val result : ('a -> 'b) -> ('e -> 'b) -> ('a, 'e) result -> 'b
(** Global alias for {!Result.result}. *)

