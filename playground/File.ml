
type flag = [
  | `Read       (* Open for reading. *)
  | `Write      (* Open for writing. *)
  | `Append     (* Open for appending: always write at end of file. *)
  | `Create     (* Create the file if it does not exist. *)
  | `Truncate   (* Empty the file if it already exists. *)
  | `Exclusive  (* Fail if Open_creat and the file already exists. *)
  | `Binary     (* Open in binary mode (no conversion). *)
  | `Text       (* Open in text mode (may perform conversions). *)
  | `Nonblock   (* Open in non-blocking mode. *)
]

type read
type write

type 'm mode =
  | Read  : read mode
  | Write : write mode

module Channel' = struct

  type 'a t =
    | I : in_channel  -> read t
    | O : out_channel -> write t

  let make (type m) (mode : m mode) path : m t =
    match mode with
    | Read -> I (open_in path)
    | Write -> O (open_out path)

  let read_line (I chan) =
    input_line chan

  let close (type m) (self : m t) : unit =
    match self with
    | I c -> close_in c
    | O c -> close_out c
end


module Channel : sig
end = struct
  type input  = in_channel
  type output = out_channel


  let read_line c =
    try Some (input_line c) with
      End_of_file -> None
end

let open_file = Channel.make_input
let create_file = Channel.make_output


