
module Array  = El_array
module Either = El_either
module Exn    = El_exn
module Float  = El_float
module Fn     = El_fn
module Int    = El_int
module List   = El_list
module Map    = El_map
module Option = El_option
module Vector = El_persistent_vector
module Result = El_result
module Stream = El_stream
module String = El_string
module Void   = El_void

include El_base

(* Public Fn *)
let (%)  = El_fn.(%)
let (|>) = El_fn.(|>)
let id   = El_fn.id
let flip = El_fn.flip

(* Public List *)
let cons = El_list.cons

(* Public Option *)
let some   = El_option.some
let none   = El_option.none
let guard  = El_option.guard
let option = El_option.option

(* Public Exn *)
let fail  = El_exn.fail

(* Public Void *)
type void = El_void.t

(* Public Result *)
let is_ok    = El_result.is_ok
let is_error = El_result.is_error


