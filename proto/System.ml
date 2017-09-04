
module Stdlib = Proto_shadow_stdlib

module IO = struct
  module File = struct
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
          Buffer.add_subbytes buffer buf 0 len;
          loop ()
        end in
      loop ();
      let str = Buffer.contents buffer in
      if Stdlib.String.length str = 0 then None else Some str

    let write_char (Out {chan}) c =
      Pervasives.output_char chan c

    (* module Lines = struct *)

    (*   let iter (In {chan}) = *)
    (*     let rec next chan = *)
    (*       try Some (input_line chan, chan) *)
    (*       with End_of_file -> None in *)
    (*     Iter (chan, next) *)

    (*   include Iter.Input.Make0(struct *)
    (*       type nonrec t = input t *)
    (*       type item = string *)
    (*       let iter = iter *)
    (*     end) *)
    (* end *)

    (* module Chars = struct *)

    (*   let iter (In {chan}) = *)
    (*     let rec next chan = *)
    (*       try Some (input_char chan, chan) *)
    (*       with End_of_file -> None in *)
    (*     Iter (chan, next) *)

    (*   include Iter.Input.Make0(struct *)
    (*       type nonrec t = input t *)
    (*       type item = char *)
    (*       let iter = iter *)
    (*     end) *)
    (* end *)
  end

  let stdin  = File.of_in  Pervasives.stdin
  let stdout = File.of_out Pervasives.stdout
  let stderr = File.of_out Pervasives.stderr

end

let time f =
  let t = Unix.gettimeofday () in
  let r = f () in
  Printf.eprintf "Elapsed time: %f sec\n" (Unix.gettimeofday () -. t);
  r

