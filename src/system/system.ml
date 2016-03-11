
open Base

let time ?fmt f x =
  let t0 = Unix.gettimeofday () in
  let fx = f x in
  let t1 = Unix.gettimeofday () -. t0 in
  match fmt with
  | Some fmt -> Printf.eprintf (fmt t1)
  | None     -> Printf.eprintf "Elapsed time: %f sec\n" t1;
  fx

