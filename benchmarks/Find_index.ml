open Proto

module L = Data.List
module A = Data.Array

module type Foldk = sig
  type 'a t
  val foldk : ('a -> 'r -> ('r -> 'r) -> 'r) -> 'r -> 'a t -> 'r
end


module Baseline = struct
  let find_index predicate self =
    let rec go i xs =
      match xs with
      | [] -> None
      | x :: xs ->
        if predicate x then Some i
        else go (i + 1) xs in
    go 0 self
end


module Foldk_either(M : Foldk) = struct
  type s = { i : int; r : int option }

  let find_index predicate self =
    let step a state continue =
      match state with
      | Left i when predicate a -> Right i
      | Left i -> continue (Left (i + 1))
      | _ -> undefined () in
    match M.foldk step (Left 0) self with
    | Left _ -> None
    | Right i -> Some i
end


module Inspect = struct
  let inspect f empty self =
    match self with
    | [] -> empty
    | x :: xs -> f x xs

  let find_index predicate self =
    let rec go i state =
      inspect
        (fun a state' ->
           if predicate a then Some i
           else go (i + 1) state')
        None
        state in
    go 0 self
end


let input =
  let n = 50_000 in
  Array.(to_list (append
                    (init n (fun x -> x + 1))
                    (init (n - (n / 2)) (fun x -> x))))

let run find_index =
  assert (find_index (equal 0) input = Some 50_000)


let () =
  let open Core_bench.Std in
  let bench name = Bench.Test.create ~name in
  Core.Command.run (Bench.make_command [
      bench "Baseline"            (fun () -> run Baseline.find_index);
      bench "Foldk"               (fun () -> run Foldk.find_index);
      bench "Inspect"             (fun () -> run Inspect.find_index);
    ])


(*
  Estimated testing time 30s (3 benchmarks x 10s). Change using -quota SECS.
  ┌──────────┬──────────┬─────────────┬──────────┬──────────┬────────────┐
  │ Name     │ Time/Run │     mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
  ├──────────┼──────────┼─────────────┼──────────┼──────────┼────────────┤
  │ Baseline │ 438.99us │       7.00w │          │          │     90.53% │
  │ Foldk    │ 484.92us │ 100_008.53w │    1.91w │    1.91w │    100.00% │
  │ Inspect  │ 414.56us │       7.00w │          │          │     85.49% │
  └──────────┴──────────┴─────────────┴──────────┴──────────┴────────────┘
*)

