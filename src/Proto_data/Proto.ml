include Kernel

module Control = Control
module System = System

module Exception = Control.Exception
module Function = Control.Function

module Data = struct
  module Array      = Array
  module Dict       = Dict
  module Either     = Either
  module List       = List
  module Iter       = Iter
  module Fold       = Fold
  module Option     = Option
  module Ref        = Ref
  module Result     = Result
  module Set        = Set
  module Stream     = Stream
  module Collection = Collection
  module String     = String
  module Tuple      = Tuple
  module Void       = Void
end

type 'a iter = 'a Data.Iter.t

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


module IO = struct
  open Data.Iter.Public

  let file path mode =
    undefined ()

  module Chan = struct
    type 'mode t
    type flag = [
      | `read       (* Open for reading. *)
      | `write      (* Open for writing. *)
      | `append     (* Open for appending: always write at end of file. *)
      | `create     (* Create the file if it does not exist. *)
      | `truncate   (* Empty the file if it already exists. *)
      | `exclusive  (* Fail if Open_creat and the file already exists. *)
      | `binary     (* Open in binary mode (no conversion). *)
      | `text       (* Open in text mode (may perform conversions). *)
      | `nonblock   (* Open in non-blocking mode. *)
    ]

    let flag_to_std_flag = function
      | `read       -> Open_rdonly
      | `write      -> Open_wronly
      | `append     -> Open_append
      | `create     -> Open_creat
      | `truncate   -> Open_trunc
      | `exclusive  -> Open_excl
      | `binary     -> Open_binary
      | `text       -> Open_text
      | `nonblock   -> Open_nonblock

    (* let make path flags = *)
    (*   let flags = Stdlib.List.map flag_to_std_flag flags in *)
    (*   open_in_gen flags 0 path *)


    let read_byte  chan = Stdlib.input_byte   chan
    let read_char  chan = Stdlib.input_char   chan
    let read_value chan = Stdlib.input_value  chan
    let read_line  chan = Stdlib.input_line   chan

    let write_byte   x chan = Stdlib.output_byte   chan x
    let write_bytes  x chan = Stdlib.output_bytes  chan x
    let write_char   x chan = Stdlib.output_char   chan x
    let write_value  x chan = Stdlib.output_value  chan x
    let write_string x chan = Stdlib.output_string chan x
    let write_line   x chan = Stdlib.output_string chan (x ^ "\n")
  end

  module File = struct
    type path = string

    let with_input path =
      bracket (fun () -> open_in path) close_in

    let with_output path =
      bracket (fun () -> open_out path) close_out

  end
end

