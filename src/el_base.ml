
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

let time ?fmt f x =
  let t0 = Unix.gettimeofday () in
  let fx = f x in
  let t1 = Unix.gettimeofday () -. t0 in
  match fmt with
  | Some fmt -> Printf.eprintf (fmt t1)
  | None     -> Printf.eprintf "Elapsed time: %f sec\n" t1;
  fx

(* Printing and Formatting *)

let print = print_endline
let fmt = Printf.sprintf

(* Numeric Primitives *)

let even n = n mod 2 = 0
let odd  n = n mod 2 = 1

(* Channel *)

let output_line chan line =
  output_string chan (line ^ "\n")

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type Type = sig
  type t
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


