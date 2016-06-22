
module Iter = Data_iter

open Base

module Chan = struct
  type input  = { chan : in_channel  }
  type output = { chan : out_channel }

  type 'a t =
    | In  : input  -> input t
    | Out : output -> output t

  let open_in  name = In  { chan = open_in  name }
  let open_out name = Out { chan = open_out name }

  let of_in  chan = In  { chan }
  let of_out chan = Out { chan }

  let read_char (In {chan}) =
    try Some (Pervasives.input_char chan)
    with End_of_file -> None

  let read_line (In {chan}) =
    try Some (Pervasives.input_line chan)
    with End_of_file -> None

  (* Based on Janestreet's Core library:
     https://github.com/janestreet/core/blob/master/COPYRIGHT.txt *)
  let read_all (In {chan}) =
    (* We use 65536 because that is the size of OCaml's IO buffers. *)
    let buf_size = 65536 in
    let buf = Bytes.create buf_size in
    let buffer = Buffer.create buf_size in
    let rec loop () =
      let len = input chan buf 0 (Bytes.length buf) in
      if len > 0 then begin
        Buffer.add_substring buffer buf 0 len;
        loop ()
      end in
    loop ();
    let str = Buffer.contents buffer in
    if String.length str = 0 then None else Some str

  let write_char (Out {chan}) c =
    Pervasives.output_char chan c

  module Lines = Iter.Input.Make0(struct
      type nonrec t = input t
      type item = string

      let iter (In {chan}) =
        let rec next chan =
          try Some (input_line chan, chan)
          with End_of_file -> None in
        Iter (chan, next)
    end)

  module Chars = Iter.Input.Make0(struct
      type nonrec t = input t
      type item = char

      let iter (In {chan}) =
        let rec next chan =
          try Some (input_char chan, chan)
          with End_of_file -> None in
        Iter (chan, next)
    end)
end

let stdin  = Chan.of_in  Pervasives.stdin
let stdout = Chan.of_out Pervasives.stdout
let stderr = Chan.of_out Pervasives.stderr

