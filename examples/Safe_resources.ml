
let pass = fun () -> ()

let output_line str chan =
  Pervasives.output_string chan (str ^ "\n")

let open_in path =
  print "open_in";
  Pervasives.open_in path

let open_out path =
  print "open_out";
  Pervasives.open_out path

let close_in chan =
  print "close_in";
  Pervasives.close_in chan

let close_out chan =
  print "close_out";
  Pervasives.close_out chan

let finally f g x =
   try
     let res = f x in
     g x;
     res
   with e ->
     g x;
     raise e

let with_open_in name f =
   let c = open_in name in
   finally f close_in_noerr c

(* -- // -- *)

(* XXX: enumBracket *)

module Iter = struct


  (* let map f (Iter i) = *)
  (*   let next yield = *)
  (*     i.next (fun a -> yield (f a)) in *)
  (*   Iter { i with next } *)

  (* let take n (Iter iter) = *)
  (*   if n = 0 then empty else *)
  (*   if n < 0 then raise (Invalid_argument (format "take %d" n)) else *)
  (*   let next yield finish (s, i) = *)
  (*     if i <= 0 then finish () *)
  (*     else iter.next *)
  (*       (fun a s' -> yield a (s', i - 1)) *)
  (*       (fun () -> finish ()) *)
  (*       s in *)
  (*   Iter { iter with init = (fun () -> iter.init (), n); next } *)

  (* let fold f r (Iter iter) = *)
  (*   let rec go r s = *)
  (*     iter.next (fun a -> go (f a r)) (fun () -> r) s in *)
  (*   go r (iter.init ()) *)

  (* let to_list (Iter iter) = *)
  (*   let rec go acc = *)
  (*     iter.next *)
  (*       (fun a -> go (a :: acc)) *)
  (*       (fun () -> iter.stop (); acc) in *)
  (*   List.rev (go [] (iter.init ())) *)

  (* let of_list list = *)
  (*   let next yield finish s = *)
  (*     match s with *)
  (*     | [] -> finish () *)
  (*     | a :: rest -> yield a rest in *)
  (*   Iter { init = list; next; stop = pass } *)

  (* let to_list (Iter iter) = *)
  (*   let rec go acc = *)
  (*     iter.next *)
  (*       (fun a -> go (a :: acc)) *)
  (*       (fun s -> iter.stop s; acc) in *)
  (*   List.rev (go [] (iter.init ())) *)

  type 'a iter =
    Iter : {
      init : unit -> 's;
      next : 'r . ('a -> 's -> 'r) -> ('s -> 'r) -> 's -> 'r;
      stop : 'r . 's -> unit;
    } -> 'a iter

  let empty =
    Iter { init = pass;
           next = (fun _yield stop s -> stop s);
           stop = ignore }

  let map f (Iter i) =
    let next yield =
      i.next (fun a -> yield (f a)) in
    Iter { i with next }

  let filter predicate (Iter iter) =
    let rec next yield stop =
      iter.next
        ((fun a s' ->
            if predicate a then yield a s'
            else next yield stop s'))
        stop in
    Iter { iter with next }

  let take n (Iter iter) =
    if n = 0 then empty else
    if n < 0 then raise (Invalid_argument (format "take %d" n)) else
    let next yield stop (s, i) =
      if i <= 0 then stop (s, i)
      else iter.next
        (fun a s' -> yield a (s', i - 1))
        (fun s -> iter.stop s; stop (s, i))
        s in
    Iter { init = (fun () -> iter.init (), n);
           stop = (fun (s, _) -> iter.stop s);
           next }

  let fold f r (Iter iter) =
    let rec go r s =
      iter.next (fun a -> go (f a r)) (fun s -> iter.stop s; r) s in
    go r (iter.init ())

  let of_file path =
    let next yield stop s =
      try yield (input_line s) s with
      | End_of_file -> stop s
      | exn -> stop s; raise exn in
    Iter { init = (fun () -> open_in path); next; stop = close_in }

  let to_file path (Iter iter) =
    let rec go chan count =
      iter.next
        (fun item -> output_line item chan; go chan (count + 1))
        (fun s -> close_out chan; iter.stop s; count) in
    go (open_out path) 0 (iter.init ())

  let of_list list =
    let next yield stop s =
      match s with
      | [] -> stop s
      | a :: s' -> yield a s' in
    Iter { init = list; next; stop = ignore }

  let to_list' (Iter iter) =
    let rec go acc =
      iter.next
        (fun a -> go (a :: acc))
        (fun s -> iter.stop s; acc) in
    List.rev (go [] (iter.init ()))
end


let () =
  let count =
    Iter.of_file "/etc/hosts"
    |> Iter.map (fun x -> String.uncapitalize_ascii x)
    |> Iter.to_file "/tmp/hosts" in
  print (format "wrote %d lines" count)

(* let () = *)
(*   "/etc/hosts" *)
(*   |> Iter.of_file *)
(*   |> Iter.map (fun line -> String.length line) *)
(*   |> Iter.filter (fun n -> n > 1) *)
(*   |> Iter.map (fun n -> Int.to_string n) *)
(*   |> Iter.take 3 *)
(*   |> Iter.to_list *)
(*   |> List.iter print *)

(* let () = *)
(*   "/etc/hosts" *)
(*   |> Iter.of_file *)
(*   |> Iter.map (fun line -> String.length line) *)
(*   |> Iter.filter (fun n -> n > 1) *)
(*   |> Iter.map (fun n -> Int.to_string n) *)
(*   |> Iter.take 3 *)
(*   |> Iter.to_file "/tmp/xxx-out" *)


