
module Baseline = struct
  let count_until_0 input =
    let rec go i =
      if i = Array.length input then i else
      let x = Array.unsafe_get input i in
      if x = 0 then i
      else go (i + 1) in
    go 0
end

module Foldk = struct
  let foldk f init self =
    let n = Array.length self in
    let rec go i r =
      if i = n then r
      else f (Array.unsafe_get self i) r (fun r' -> go (i + 1) r') in
    go 0 init

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

module Iterable_array = Iterable.Make(struct
    type 'a t = 'a array
    type 'a state = int

    let init a = 0

    let next state f r self =
      if state = Array.length self then r
      else f (Array.unsafe_get self state) (state + 1)
  end)

let n = 500_000
let input =
  Array.(append (init n (fun x -> x + 1))
           (init (n / 2) (fun x -> x)))

let run f () =
  let actual = f input in
  assert (actual = n)

let () =
  let open Core_bench.Std in
  let bench name = Bench.Test.create ~name in
  Core.Command.run (Bench.make_command [
      bench "Baseline"   (run Baseline.count_until_0);
      bench "Foldk"      (run Foldk.count_until_0);
      bench "Iterable"   (run Iterable_array.count_until_0);
    ])

