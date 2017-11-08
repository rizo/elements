
module Baseline = struct
  let rec map f self =
    match self with
    | x :: xs -> f x :: map f xs
    | [] -> []

  let bench input () =
    map (fun x -> x + 100) input
end

module Baseline_tco = struct
  let map f self =
    let rec go acc input =
      match input with
      | x :: xs -> go (f x :: acc) xs
      | [] -> acc in
    go [] self

  let bench input () =
    map (fun x -> x + 100) input
end

module Iter = struct
  type 'a iter =
      Iter : {
        init : 'cursor;
        next : 'r . ('a -> 'cursor -> 'r) -> 'r -> 'cursor -> 'r;
      } -> 'a iter

  let fold_state s0 f r0 next =
    let rec go r s =
      next (fun a -> go (f a r)) r s in
    go r0 s0

  let fold f r (Iter i) =
    fold_state i.init f r i.next

  let to_list_reversed self =
    fold (fun x xs -> x :: xs) [] self

  let map f (Iter i) =
    let next yield =
      i.next (fun a -> yield (f a))
    in
    Iter { i with next }

  let iter l =
      let next yield empty cursor =
        match cursor with
        | []    -> empty
        | x::xs -> yield x xs in
      Iter { init = l; next }

  let bench input () =
    to_list_reversed (map (fun x -> x + 100) (iter input))
end


let n = 500_000
let input =
  Array.to_list (Array.init n (fun x -> x + 1))


let () =
  let open Core_bench.Std in
  let bench name = Bench.Test.create ~name in
  Core.Command.run (Bench.make_command [
      bench "Baseline"       (Baseline.bench input);
      bench "Baseline_tco"   (Baseline_tco.bench input);
      bench "Iter"           (Iter.bench input);
    ])

