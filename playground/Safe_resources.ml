
(* Helper definitions *)

let pass = fun () -> ()

let output_line str chan =
  Pervasives.output_string chan (str ^ "\n")

let open_in path =
  print (format "> open_in %s" path);
  Pervasives.open_in path

let open_out path =
  print (format "> open_out %s" path);
  Pervasives.open_out path

let close_in chan =
  print "> close_in";
  Pervasives.close_in chan

let close_out chan =
  print "> close_out";
  Pervasives.close_out chan


let bracket init (stop : 'a -> unit) f =
  let x = init () in
  try
    let r = f x in
    stop x;
    r
  with exn ->
    stop x;
    raise exn


let with_input_file path =
  bracket (fun () -> open_in path) close_in

let with_output_file path =
  bracket (fun () -> open_out path) close_out


(* Iterators *)

type 'a iter =
  Iter : {
    init : unit -> 's;
    next : 'r . ('a -> 's -> 'r) -> 'r -> 's -> 'r;
    free : 's -> unit
  } -> 'a iter

module Iter = struct
  type 'a t = 'a iter

  let empty =
    Iter { init = pass;
           next = (fun _ r _ -> r);
           free = ignore }


  (* Producers *)

  let of_list l =
    let[@inline] next yield r s =
      match s with
      | [] -> r
      | a :: s' -> yield a s' in
    Iter { init = (fun () -> l); next; free = ignore }

  let of_file path =
    let next yield r s =
      try yield (input_line s) s
      with End_of_file -> r in
    Iter { init = (fun () -> open_in path);
           next; free = close_in }


  (* Transducers *)

  let map f (Iter iter) =
    let next k =
      iter.next (fun a -> k (f a)) in
    Iter { iter with next }

  let filter predicate (Iter iter) =
    let[@inline] rec next yield r s =
      iter.next
        ((fun a s' ->
            if predicate a then yield a s'
            else next yield r s')) r s in
    Iter { iter with next }

  let take n (Iter iter) =
    if n = 0 then empty else
    if n < 0 then raise (Invalid_argument (format "take %d" n)) else
    let[@inline] next yield r (s, i) =
      if i <= 0 then r
      else iter.next (fun a s' -> yield a (s', i - 1)) r s in
    Iter { init = (fun () -> (iter.init (), n));
           free = (fun (s, _) -> iter.free s);
           next }


  (* Consumers *)

  let[@inline] fold f r (Iter iter) =
    let[@inline] rec loop r s =
      iter.next (fun a s' -> loop (f a r) s') r s in
    bracket iter.init iter.free (loop r)

  let to_channel chan (Iter iter) =
    let rec loop count =
      iter.next
        (fun str ->
           output_string chan str;
           loop (count + 1))
        count in
    bracket iter.init iter.free (loop 0)

  let to_file_count path iter =
    with_output_file path (fun chan -> to_channel chan iter)
end


let exhaustive_run () =
  "/etc/hosts"
  |> Iter.of_file
  |> Iter.map (fun line -> String.length line)
  |> Iter.filter (fun n -> n > 1)
  |> Iter.map (fun n -> Int.to_string n ^ "\n")
  |> Iter.to_file_count "/tmp/hosts"
  |> format "wrote %d lines"
  |> print

let early_termination () =
  "/etc/hosts"
  |> Iter.of_file
  |> Iter.map (fun line -> String.length line)
  |> Iter.filter (fun n -> n > 1)
  |> Iter.map (fun n -> Int.to_string n ^ "\n")
  |> Iter.take 3
  |> Iter.to_file_count "/tmp/hosts"
  |> format "wrote %d lines"
  |> print

let exn_exhaustive_run () =
  "/etc/hosts"
  |> Iter.of_file
  |> Iter.map (fun line -> String.length line / 0)
  |> Iter.filter (fun n -> n > 1)
  |> Iter.map (fun n -> Int.to_string n ^ "\n")
  |> Iter.to_file_count "/tmp/hosts"
  |> format "wrote %d lines"
  |> print

let exn_early_termination () =
  "/etc/hosts"
  |> Iter.of_file
  |> Iter.map (fun line -> String.length line / 0)
  |> Iter.filter (fun n -> n > 1)
  |> Iter.map (fun n -> Int.to_string n ^ "\n")
  |> Iter.take 3
  |> Iter.to_file_count "/tmp/hosts"
  |> format "wrote %d lines"
  |> print

