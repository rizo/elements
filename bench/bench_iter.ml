
(* Iter option *)

let array_iter_option a =
  let len = Array.length a in
  let next state =
    if state < len
    then Some (Array.unsafe_get a state, state + 1)
    else None in
  (0, next)

let fold_option f acc iter =
  let (s0, next) = iter in
  let rec loop acc s =
    match next s with
    | None -> acc
    | Some (a, s') -> loop (f acc a) s' in
  loop acc s0


(* Iter option record *)

type ('a, 's) iter_0 = {init : 's ; next : 's -> ('a * 's) option}

let array_iter_option_record a =
  let len = Array.length a in
  let next state =
    if state < len
    then Some (Array.unsafe_get a state, state + 1)
    else None in
  {init=0; next}

let fold_option_record f acc iter =
  let { init = s0; next } = iter in
  let rec loop acc s =
    match next s with
    | None -> acc
    | Some (a, s') -> loop (f acc a) s' in
  loop acc s0


(* Iter stop tuple *)

let fold_stop f acc iter =
  let (s0, next, stop) = iter in
  let rec loop acc s =
    if stop s then acc
    else
      let a, s' = next s in
      loop (f acc a) s' in
  loop acc s0

let array_iter_stop a =
  let len = Array.length a in
  let next state = Array.unsafe_get a state, state + 1 in
  let stop state = (state = len) in
  (0, next, stop)


(* Iter stop with record *)

type ('a, 's) iter = {init : 's ; next : 's -> ('a * 's); stop : 's -> bool}

let fold_stop_record f acc iter =
  let {init = s0; next; stop} = iter in
  let rec loop acc s =
    if stop s then acc
    else
      let a, s' = next s in
      loop (f acc a) s' in
  loop acc s0

let array_iter_stop_record a =
  let len = Array.length a in
  let next state = Array.unsafe_get a state, state + 1 in
  let stop state = (state = len) in
  {init=0; next; stop}

let id x = x

let () =
  let open Core_bench.Std in
  print_endline "Iterators implementation benchmark.";
  let bench  n   = Bench.Test.create ~name:n in
  let benchi n a = Bench.Test.create_indexed ~name:n ~args:a in
  Core.Command.run (Bench.make_command [
      bench "array_fold"        (fun () -> Array.fold_left    (+) 0 input);
      bench "iter_option"        (fun () -> fold_option        (+) 0 (array_iter_option input));
    ])

