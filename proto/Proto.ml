include Kernel

module Control = Control

module Exception = Control.Exception
module Function = Control.Function

module Data = struct
  module Array  = Array
  module Dict   = Dict
  module Either = Either
  module List   = List
  module Option = Option
  module Ref    = Ref
  module Result = Result
  module Set    = Set
  module Stream = Stream
  module String = String
  module Tuple  = Tuple
  module Void   = Void
end

module Option = Data.Option
module Result = Data.Result

type void = Data.Void.t

(* Tuples *)
type ('a, 'b) pair = ('a, 'b) Data.Tuple.pair

(* Option *)
let some = Data.Option.some
let none = Data.Option.none
let is_some = Data.Option.is_some
let is_none = Data.Option.is_none
let option = Data.Option.case
let or_fail = Data.Option.or_fail

(* Result *)
let ok = Data.Result.ok
let error = Data.Result.error
let is_ok = Data.Result.is_ok
let is_error = Data.Result.is_error
let result = Data.Result.case

