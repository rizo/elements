include Kernel

module Control = Control

module Exception = Control.Exception
module Function = Control.Function

module Data = struct
  module Array  = Array
  module Either = Either
  module List   = List
  module Dict   = Dict
  module Option = Option
  module Result = Result
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
let _1 = Data.Tuple._1
let _2 = Data.Tuple._2

(* Option *)
let some = Data.Option.some
let none = Data.Option.none
let is_some = Data.Option.is_some
let is_none = Data.Option.is_none
let option = Data.Option.option
let ( or ) = Data.Option.( or )
let or_else = Data.Option.or_else

(* Result *)
let ok = Data.Result.ok
let error = Data.Result.error
let is_ok = Data.Result.is_ok
let is_error = Data.Result.is_error
let result = Data.Result.result
