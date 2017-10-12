
module Baseline = struct
  let count_until_0 input =
    let rec go i state =
      match state with
      | [] -> i
      | x :: xs ->
          if x = 0 then i
          else go (i + 1) xs in
    go 0 input
end

module Foldk = struct
  let foldk f =
    let rec go b self =
      match self with
      | [] -> b
      | a :: self' -> f a b (fun b' -> go b' self') in
    go

  let count_until_0 input =
    foldk
      (fun x i continue ->
         if x = 0 then i
         else continue (i + 1))
      0 input
end

module Iterable = struct
  module type Base = sig
    type 'a t
    type 'a state

    val init : 'a t -> 'a state
    val next : 'a state -> ('a -> 'a state -> 'r) -> 'r -> 'a t -> 'r
  end

  module Make(B : Base) = struct
    let count_until_0 input =
      let rec go count state =
        B.next state
          (fun x state' ->
             if x = 0 then count
             else go (count + 1) state')
          count input in
      go 0 (B.init input)
  end
end

module Iterable_list = Iterable.Make(struct
    type 'a t = 'a list
    type 'a state = 'a t

    let init a = a

    let next state f r self =
      match state with
      | [] -> r
      | x :: xs -> f x xs
  end)

module Iterable' = struct
  module type Base = sig
    type 'a t
    type 'a state

    val init : 'a t -> 'a state
    val next : 'a state -> ('a -> 'a state -> 'r) -> 'r -> 'r
  end

  module Make(B : Base) = struct
    let count_until_0 input =
      let rec go count state =
        B.next state
          (fun x state' ->
             if x = 0 then count
             else go (count + 1) state')
          count in
      go 0 (B.init input)
  end
end

module Iterable_list' = Iterable'.Make(struct
    type 'a t = 'a list
    type 'a state = 'a t

    let init a = a

    let next state f r =
      match state with
      | [] -> r
      | x :: xs -> f x xs
  end)

let n = 100_000
let input =
  Array.(append (init n (fun x -> x + 1))
           (init (n / 2) (fun x -> x)))
  |> Array.to_list

let run f () =
  let actual = f input in
  assert (actual = n)

let () =
  let open Core_bench.Std in
  let bench name = Bench.Test.create ~name in
  Core.Command.run (Bench.make_command [
      bench "Baseline"   (run Baseline.count_until_0);
      bench "Foldk"      (run Foldk.count_until_0);
      bench "Iterable"   (run Iterable_list.count_until_0);
      bench "Iterable'"  (run Iterable_list'.count_until_0);
    ])

