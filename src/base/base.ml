
type ordering = LT | EQ | GT

type ('a, 'e) result =
  | Ok    of 'a
  | Error of 'e

let ok x = Ok x
let error x = Error x

type ('a, 'b) either =
  | Left  of 'a
  | Right of 'b

let either f g x =
  match x with
  | Left  l -> f l
  | Right r -> g r

let discard _ = ()

(* Printing and Formatting *)

let print = print_endline
let fmt = Printf.sprintf

(* Numeric Primitives *)

let even n = n mod 2 = 0
let odd  n = n mod 2 = 1

(* Channel *)

let output_line chan line =
  output_string chan (line ^ "\n")

(* Common signatures *)

module type Type = sig
  type t
end

module type Ord = sig
  type t
  val compare : t -> t -> int
end

module type Show = sig
  type t [@@deriving show]
end

module type Functor = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Id = struct
  module Make (X : Type) = X
  type 'a t = 'a
  let map f x = f x
end

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Lazy = struct
  include Lazy
  let (!) = Lazy.force
end

module Log = struct
  let out level msg =
    output_line stderr (fmt "%s: %s"  level msg); flush stderr
  let info msg = out "info" msg
  let error msg = out "error" msg
  let warning msg = out "warning" msg
end

(* Exn base *)
let fail msg = raise (Failure msg)

(* Option base *)
let some x = Some x
let none   = None
let guard f x =
  try Some (f x)
  with _ -> None

(* Result base *)
let is_ok    = function Ok _ -> true  | Error _ -> false
let is_error = function Ok _ -> false | Error _ -> true

(* Fn base *)
let compose f g = fun x -> f (g x)
let (<<) f g = compose f g
let (>>) g f = compose f g
let id x = x
let flip f x y = f y x

