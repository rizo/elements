
type ('a, 'e) result =
  | Ok    of 'a
  | Error of 'e

let ok x = Ok x
let error x = Error x

type ('a, 'b) either =
  | Left  of 'a
  | Right of 'b

let either f g x =
  match x with
  | Left  l -> f l
  | Right r -> g r

let discard _ = ()

let time f x =
  let t = Unix.gettimeofday () in
  let fx = f x in
  Printf.printf "Elapsed time: %f sec\n"
    (Unix.gettimeofday () -. t);
  fx

(* Printing and Formatting *)

let print = print_endline
let fmt = Printf.sprintf

(* Numeric Primitives *)

let even n = n mod 2 = 0
let odd  n = n mod 2 = 1

(* Channel *)

let output_line chan line =
  output_string chan (line ^ "\n")

