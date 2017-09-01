(*---------------------------------------------------------------------------
   Copyright (c) 2017 Rizo Isrof. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

include Kernel

module Control = Control

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

(* Result *)
let ok = Data.Result.ok
let error = Data.Result.error
let is_ok = Data.Result.is_ok
let is_error = Data.Result.is_error
let result = Data.Result.result

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Rizo Isrof

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
