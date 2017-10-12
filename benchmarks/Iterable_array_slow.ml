
module A = Proto__Array
let scoped = Proto.Control.scoped

module Baseline = struct
  let find p a =
    let n = A.length a in
    let rec loop i =
      if i = n then None
      else let x = A.unsafe_get a i in if p x then Some x
      else loop (i + 1) in
    loop 0

  let bench n a =
    assert (find ((=) n) a = Some n)
end

module Iter_exn = struct
  exception Stop

  let array self =
    let n = A.length self in
    fun i ->
      if i = n then raise Stop
      else (A.unsafe_get self i, (i + 1))

  let find predicate iter =
    let rec go index =
      try
        let a, index' = iter index in
        if predicate a then Some a
        else go index'
      with Stop -> None
    in
      go 0

  let bench n a =
    assert (find ((=) n) (array a) = Some n)
end

module Iter_option = struct
  let array self =
    let n = A.length self in
    fun i ->
      if i = n then None
      else Some (A.unsafe_get self i, (i + 1))

  let find predicate iter =
    let rec go index =
      match iter index with
      | Some (a, _) when predicate a -> Some a
      | Some (_, index') -> go index'
      | None -> None
    in
      go 0

  let bench n a =
    assert (find ((=) n) (array a) = Some n)
end

module Iter_k_state_record = struct
  type 'a state = {
    array : 'a array;
    index : int;
  }

  let view f empty { array; index } =
    if index = A.length array then empty
    else f (A.unsafe_get array index) { array; index = index + 1 }

  let rec find predicate state =
    view
      (fun a state' ->
         if predicate a then Some a
         else find predicate state')
      None
      state

  let bench n a =
    assert (find ((=) n) { array = a; index = 0 } = Some n)
end

module Iter_k_stack_tuple = struct
  type ('a, 's, 'r) iter = ('a -> 's -> 'r) -> 'r -> 's -> 'r

  let view =
    fun f empty (self, i) ->
      if i = A.length self then empty
      else f (A.unsafe_get self i) (i + 1)

  let rec find predicate (input, index) =
    view
      (fun a index' ->
         if predicate a then Some a
         else find predicate (input, index'))
      None
      (input, index)

  let bench n a =
    assert (find ((=) n) (a, 0) = Some n)
end

module Iter_k_stack_many = struct
  type ('a, 's, 'r) iter = ('a -> 's -> 'r) -> 'r -> 's -> 'r

  let view =
    fun f empty self i ->
      if i = A.length self then empty
      else f (A.unsafe_get self i) (i + 1)

  let rec find predicate input index =
    view
      (fun a index' ->
         if predicate a then Some a
         else find predicate input index')
      None
      input
      index

  let bench n a =
    assert (find ((=) n) a 0 = Some n)
end

let n = 20_000 - 1
let input = Array.init 1_000_000 (fun x -> x)

let () =
  let open Core_bench.Std in
  Printf.printf "Benchmark for Foldable.Base implementations (n = %d).\n" n;
  let bench n = Bench.Test.create ~name:n in
  Core.Command.run (Bench.make_command [
      bench "Baseline"              (fun () -> Baseline.bench n input);
      bench "Iter_option"           (fun () -> Iter_option.bench n input);
      bench "Iter_k_state_record"   (fun () -> Iter_k_state_record.bench n input);
      bench "Iter_exn"              (fun () -> Iter_exn.bench n input);
      bench "Iter_k_stack_tuple"    (fun () -> Iter_k_stack_tuple.bench n input);
      bench "Iter_k_stack_many"     (fun () -> Iter_k_stack_many.bench n input);
    ])

