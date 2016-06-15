
open Base

type input  = { chan : in_channel  }
type output = { chan : out_channel }

module File : sig
  type 'a t

  val open_in  : string -> input t

  val open_out : string -> output t

  val stdin : input t

  val stdout : output t

  val stderr : output t

  val read_char : input t -> char option

  val read_line : input t -> string option

  val read_all : input t -> string option

  val write_char : output t -> char -> unit
end = struct
  type 'a t =
    | In  : input  -> input t
    | Out : output -> output t

  let open_in  name = In  { chan = open_in  name }
  let open_out name = Out { chan = open_out name }

  let stdin  = In  { chan = Pervasives.stdin  }
  let stdout = Out { chan = Pervasives.stdout }
  let stderr = Out { chan = Pervasives.stderr }

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

  let rec fold_lines f (init as r) (In {chan} as self) =
    match guard Pervasives.input_line chan with
    | Some line -> fold_lines f (f r line) self
    | None      -> r

  let rec iter_lines f (In {chan} as self) =
    match guard Pervasives.input_line chan with
    | Some line -> let () = f line in iter_lines f self
    | None      -> ()

end

let stdin  = File.stdin
let stdout = File.stdout
let stderr = File.stderr

