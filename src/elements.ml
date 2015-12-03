
module Array  = El_array
module Exn    = El_exn
module Fn     = El_fn
module List   = El_list
module Map    = El_map
module Option = El_option
module String = El_string
module Void   = El_void

include El_base

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

