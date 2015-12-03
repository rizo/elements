
include El_base

module Array  = El_array
module Exn    = El_exn
module Fn     = El_fn
module List   = El_list
module Map    = El_map
module Option = El_option
module String = El_string
module Void   = El_void

(* Public Fn *)
let (%)  = Fn.(%)
let (|>) = Fn.(|>)
let id   = Fn.id
let flip = Fn.flip

(* Public List *)
let cons = List.cons

(* Public Option *)
let some   = Option.some
let none   = Option.none
let guard  = Option.guard
let option = Option.option

(* Public Exn *)
let fail  = Exn.fail

(* Public Void *)
type void = Void.t



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

