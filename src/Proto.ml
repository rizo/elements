
module Data    = Data
module Control = Control

include Base

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

(* Result *)
let ok = Data.Result.ok
let error = Data.Result.error
let is_ok = Data.Result.is_ok
let is_error = Data.Result.is_error
let result = Data.Result.result
