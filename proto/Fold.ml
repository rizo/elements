
(* Builder *)
(* Reducer *)
(* Collectable *)


open Kernel
open Iter.Public

module Public = struct
  type ('a, 'b) fold =
      Fold : {
        init : unit -> 'accumulator;
        reduce : 'a -> 'accumulator -> 'accumulator;
        extract : 'accumulator -> 'b;
      } -> ('a, 'b) fold

  type ('a, 'r) fold' =
      Fold' : {
        init : unit -> 's;
        step : 'a -> 's -> 's;
        stop : 's -> 'r;
      } -> ('a, 'r) fold'
end

include Public

type ('a, 'b) t = ('a, 'b) fold

let pure b =
  Fold { init = pass;
         reduce = (fun () _ -> ());
         extract = (fun () -> b) }

let (<$>) f (Fold { init; reduce; extract }) =
  Fold { init; reduce; extract = (fun x -> f (extract x)) }

let (<*>) (Fold l) (Fold r) =
  let init () = (l.init (), r.init ()) in
  let reduce a (ls, rs) = (l.reduce a ls, r.reduce a rs) in
  let extract (ls, rs) = l.extract ls (r.extract rs) in
  Fold { init; reduce; extract }

let sum =
  Fold { init = (fun () -> 0);
         reduce = (+);
         extract = (fun x -> x) }

let length =
  Fold { init = (fun () -> 0);
         reduce = (fun a n -> n + 1);
         extract = (fun x -> x) }

let average = (/) <$> sum <*> length

let list =
  Fold { init = (fun () x -> x);
         reduce = (fun a s -> fun xs -> s (a :: xs));
         extract = (fun f -> f []) }

let file path =
  Fold { init = (fun () -> Stdlib.open_out path);
         reduce = (fun a s -> Stdlib.output_string s a; s);
         extract = close_out }

let into (Fold f) (Iter i) =
  let rec loop r s =
    i.next (fun a s' -> loop (f.reduce a r) s') r s in
  f.extract (bracket i.init i.stop (loop (f.init ())))

let into (Fold f) (Iter i) =
  let rec loop r s =
    i.next (fun a s' -> loop (f.reduce a r) s') r s in
  let r0 = f.init () in
  try
    f.extract (bracket i.init i.stop (loop r0))
  with exn ->
    f.extract r0;
    raise exn


