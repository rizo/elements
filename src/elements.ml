
module Array  = El_array
module Either = El_either
module Exn    = El_exn
module Float  = El_float
module Fn     = El_fn
module Int    = El_int
module List   = El_list
module Map    = El_map
module Option = El_option
module Result = El_result
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

