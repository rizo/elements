
module Baseline = struct
  let rec filter p self =
    match self with
    | x :: xs -> if p x then x :: filter p xs else filter p xs
    | [] -> []
end

module Baseline_tco = struct
  let filter p self =
    let rec go acc input =
      match input with
      | x :: xs -> if p x then go (x :: acc) xs else go acc xs
      | [] -> acc in
    go [] self
end

module Collection(Basic : sig
    type 'a t
    val empty : 'a t
    val add : 'a -> 'a t -> 'a t
    val case : ('a -> 'a t -> 'r) -> 'r -> 'a t -> 'r
  end) =
struct
  open Basic

  let filter p self =
    let rec go acc s =
      case (fun a s' -> if p a then go (conj a acc) s' else go acc s') acc s in
    go null self
end


module List_collection = Collection(struct
    type 'a t = 'a list

    let null = []

    let conj x xs = x :: xs

    let disj f r self =
      match self with
      | [] -> r
      | x :: xs -> f x xs
  end)


let n = 500_000
let input =
  Array.to_list (Array.init n (fun x -> x + 1))

let run f () =
  let actual = f (fun a -> a mod 2 = 0) input in
  assert (List.length actual = n / 2)


let () =
  let open Core_bench.Std in
  let bench name = Bench.Test.create ~name in
  Core.Command.run (Bench.make_command [
      bench "Baseline"       (run Baseline.filter);
      bench "Baseline_tco"   (run Baseline_tco.filter);
      bench "Collection"     (run List_collection.filter);
    ])

