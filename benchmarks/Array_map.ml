
module Baseline = struct
  let map f a =
    let l = Array.length a in
    if l = 0 then [||] else begin
      let r = Array.make l (f (Array.unsafe_get a 0)) in
      for i = 1 to l - 1 do
        Array.unsafe_set r i (f (Array.unsafe_get a i))
      done;
      r
    end

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

  let iter input =
    let next yield empty i =
      if Array.length input = i then empty
      else yield (Array.unsafe_get input i) (i + 1)
    in
      Iter { init = 0; next }

  let bench input () =
    to_list_reversed (map (fun x -> x + 100) (iter input))
end


let n = 500_000
let input =
  Array.init n (fun x -> x + 1)


let () =
  let open Core_bench.Std in
  let bench name = Bench.Test.create ~name in
  Core.Command.run (Bench.make_command [
      bench "Baseline"       (Baseline.bench input);
      bench "Iter"           (Iter.bench input);
    ])

