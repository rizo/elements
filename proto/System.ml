
open Kernel
open Iter.Public
module Stdlib = Proto_shadow_stdlib

module Path = struct
  type t = string

  let make = identity

  let to_string = identity
end

type path = Path.t

let path = Path.make


module IO = struct
  module Channel = struct
    type input  = in_channel
    type output = out_channel

    let make_input ?(binary = true) path : input =
      let flags = [Open_rdonly] in
      let flags = if binary then Open_binary :: flags else flags in
      open_in_gen flags 0o000 (Path.to_string path)

    let make_output
        ?(binary = true) ?(append = false)
        ?(exclusive = false) ?(perm = 00666) path : output =
      let flags = [Open_wronly; Open_creat] in
      let flags = (if binary then Open_binary else Open_text) :: flags in
      let flags = (if append then Open_append else Open_trunc) :: flags in
      let flags = (if exclusive then Open_excl :: flags else flags) in
      open_out_gen flags perm (Path.to_string path)

    (* Based on Janestreet's Core library:
       https://github.com/janestreet/core/blob/master/COPYRIGHT.txt *)
    let read input =
      (* We use 65536 because that is the size of OCaml's IO buffers. *)
      let buf_size = 65536 in
      let buf = Bytes.create buf_size in
      let buffer = Buffer.create buf_size in
      let rec loop () =
        let len = Stdlib.input input buf 0 (Bytes.length buf) in
        if len > 0 then begin
          Buffer.add_subbytes buffer buf 0 len;
          loop ()
        end in
      loop ();
      let str = Buffer.contents buffer in
      if Stdlib.String.length str = 0 then None else Some str

    let read_line input =
      try Some (Pervasives.input_line input)
      with End_of_file -> None

    let read_char input =
      try Some (Pervasives.input_char input)
      with End_of_file -> None

    let read_byte input =
      try Some (Pervasives.input_byte input)
      with End_of_file -> None

    let read_lines input =
      let rec next yield r s =
        try yield (Stdlib.input_line s) s
        with End_of_file -> r in
      Iter { init = (fun () -> input); next; stop = ignore }

    let iter = read_lines

    let read_chars input =
      let rec next yield r s =
        try yield (Stdlib.input_char s) s
        with End_of_file -> r in
      Iter { init = (fun () -> input); next; stop = ignore }

    let write str output =
      Stdlib.output_string output str

    let write_line str output =
      Stdlib.output_string output (str ^ "\n")

    let write_char c output =
      Stdlib.output_char output c

    let write_byte b output =
      Stdlib.output_byte output b

    let into_with_writer writer chan (Iter iter) =
      let rec loop s =
        iter.next
          (fun str s' ->
             writer str chan;
             loop s') () s in
      bracket iter.init iter.stop loop

    let write_chars   chan iter = into_with_writer write_char   chan iter
    let write_strings chan iter = into_with_writer write        chan iter
    let write_lines   chan iter = into_with_writer write_line   chan iter
    let into = write_lines

    let close_input = close_in
    let close_output = close_out
  end

  module File = struct
    let iter_with_reader reader path =
      let next yield r s =
        try yield (reader s) s
        with End_of_file -> r in
      Iter { init = (fun () -> open_in path);
             next; stop = close_in }

    let read_lines path = iter_with_reader Stdlib.input_line path
    let iter = read_lines

    let read_chars path = iter_with_reader Stdlib.input_char path

    let string_reducer path =
      Reducer {
        init = (fun () -> Stdlib.open_out path);
        reduce = (fun a s -> Channel.write_line a s; s);
        extract = Stdlib.close_out
      }

    let line_reducer path =
      Reducer {
        init = (fun () -> Stdlib.open_out path);
        reduce = (fun a s -> Channel.write_line a s; s);
        extract = Stdlib.close_out
      }

    let char_reducer path =
      Reducer {
        init = (fun () -> Stdlib.open_out path);
        reduce = (fun a s -> Channel.write_char a s; s);
        extract = Stdlib.close_out
      }

    let write_lines path iter =
      Iter.into (line_reducer path) iter

    let write_chars path iter =
      Iter.into (char_reducer path) iter

    let write_strings path iter =
      Iter.into (string_reducer path) iter
  end

  let stdin  = Stdlib.stdin
  let stdout = Stdlib.stdout
  let stderr = Stdlib.stderr

  let open_file = Channel.make_input
  let create_file = Channel.make_output

  let print = Kernel.print
end

let time f =
  let t = Unix.gettimeofday () in
  let r = f () in
  Printf.eprintf "Elapsed time: %f sec\n" (Unix.gettimeofday () -. t);
  r

