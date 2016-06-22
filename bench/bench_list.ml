
let () =
  let open Core_bench.Std in
  print_endline "Elements list implementation benchmark.";
  let input = Array.(to_list (init 100_000 (fun x -> x))) in
  let bench n = Bench.Test.create ~name:n in
  Core.Command.run (Bench.make_command [
      bench "list_fold"      (fun () -> List.fold_left          (+) 0 input);
      bench "list_iter_fold" (fun () -> Elements.Data.List.fold (+) 0 input);
    ])

