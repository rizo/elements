
module A = Proto__Array
let scoped = Proto.Control.scoped

let equal : int -> int -> bool =
  Pervasives.(=)

external unsafe_get: 'a array -> int -> 'a = "%array_unsafe_get"
external length : 'a array -> int = "%array_length"


module Baseline = struct
  let find p a =
    let n = A.length a in
    let rec loop i =
      if equal i n then None
      else let x = unsafe_get a i in if p x then Some x
      else loop (i + 1) in
    loop 0

  let bench n a =
    assert (find (equal n) a = Some n)
end

module Iter_exn = struct
  exception Stop

  let array self =
    let n = A.length self in
    fun i ->
      if i = n then raise Stop
      else (A.Unsafe.get i self, (i + 1))

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
    assert (find (equal n) (array a) = Some n)
end

module Iter_option = struct
  let array self =
    let n = A.length self in
    fun i ->
      if i = n then None
      else Some (A.Unsafe.get i self, (i + 1))

  let find predicate iter =
    let rec go index =
      match iter index with
      | Some (a, _) when predicate a -> Some a
      | Some (_, index') -> go index'
      | None -> None
    in
      go 0

  let bench n a =
    assert (find (equal n) (array a) = Some n)
end


(* -- Start Iterator -- *)

module Iter = struct
  module type Base = sig
    type 'a t
    type 'a state

    val init : 'a t -> 'a state
    val next : ('a -> 'a state -> 'r) -> 'r -> 'a state -> 'r
  end

  module Make(B : Base) = struct
    let find predicate self =
      let rec go state =
        B.next
          (fun a state' ->
             if predicate a then Some a
             else go state')
          None
          state in
      go (B.init self)
  end
end

(* -- End Iterator -- *)

module Iter_mod = struct
  include Iter.Make(struct

      type 'a t = 'a array
      type 'a state = { input : 'a array; index : int }

      let init self = { input = self; index = 0 }

      let next f empty { input; index } =
        if equal index (A.length input) then
          empty
        else
          f (unsafe_get input index)
            { input; index = index + 1 }
    end)

  let bench n a =
    assert (find (equal n) a = Some n)
end

(* Accessing record members reduces slightly the allocations but the
 * performance is not better than in Iter_mod. *)
module Iter_mod' = struct
  include Iter.Make(struct

      type 'a t = 'a array
      type 'a state = { input : 'a array; index : int }

      let init self = { input = self; index = 0 }

      let next f empty s =
        if equal s.index (A.length s.input) then empty
        else
        f (A.Unsafe.get s.index s.input)
          { s with index = s.index + 1 }
    end)

  let bench n a =
    assert (find (equal n) a = Some n)
end


(* Absolute minimum of heap allocations but slower than other inspect-based. *)
module Iter_inspect_1 = struct
  let inspect index f r self =
    if equal index (A.length self) then r
    else f (A.Unsafe.get index self)

  let rec _find predicate i self =
    inspect i
      (fun a ->
         if predicate a then Some a
         else _find predicate (i + 1) self)
      None
      self

  let find predicate self =
    _find predicate 0 self

  let bench n a =
    assert (find (equal n) a = Some n)
end


module Iter_inspect_2 = struct
  let inspect index f r self =
    if equal index (A.length self) then r
    else f (unsafe_get self index)

  let find predicate self =
    let rec go i =
      inspect i
        (fun a ->
           if predicate a then Some a
           else go (i + 1))
        None
        self in
    go 0

  let bench n a =
    assert (find (equal n) a = Some n)
end


module Iter_inspect_3 = struct
  let inspect index f r self =
    if equal index (A.length self) then r
    else f (A.Unsafe.get index self)

  type 'a state = { input : 'a array; index : int }

  let find predicate self =
    let rec go s =
      inspect s.index
        (fun a ->
           if predicate a then Some a
           else go { s with index = s.index + 1 })
        None
        s.input in
    go { input = self; index = 0 }

  let bench n a =
    assert (find (equal n) a = Some n)
end


module Foldk = struct
  let foldk f init self =
    let n = length self in
    let rec go i r =
      if equal i n then r
      else f (unsafe_get self i) r (fun r' -> go (i + 1) r') in
    go 0 init

  let find predicate input =
    foldk
      (fun a b continue ->
        if predicate a then Some a
        else continue b)
      None
      input

  let bench n a =
    assert (find (equal n) a = Some n)
end


module Iter_k_stack_many = struct
  (* type 'a t = 'a array *)
  (* type 'a iter = *)
  (*   Iter : { next : 'r . ('a -> 's -> 'r) -> 'r -> 's -> 'a t -> 'r } -> 'a iter *)

  let view =
    fun f empty i self ->
      if i = A.length self then empty
      else f (A.Unsafe.get i self) (i + 1)

  let rec find predicate index input =
    view
      (fun a index' ->
         if predicate a then Some a
         else find predicate index' input)
      None
      index
      input

  let bench n a =
    assert (find (equal n) 0 a = Some n)
end


module Iter_k_stack_tuple = struct
  let view =
    fun f empty (self, i) ->
      if i = A.length self then empty
      else f (A.Unsafe.get i self) (i + 1)

  let rec find predicate (input, index) =
    view
      (fun a index' ->
         if predicate a then Some a
         else find predicate (input, index'))
      None
      (input, index)

  let bench n a =
    assert (find (equal n) (a, 0) = Some n)
end

module Iter_k_stack_record = struct
  type 'a state = { input : 'a array; index : int }

  let view =
    fun f empty iter ->
      if iter.index = A.length iter.input then empty
      else f (A.Unsafe.get iter.index iter.input) { iter with index = iter.index + 1 }

  let rec find predicate iter =
    view
      (fun a iter' ->
         if predicate a then Some a
         else find predicate iter')
      None
      iter

  let bench n a =
    assert (find (equal n) { input = a; index = 0 } = Some n)
end

module Iter_k_stack_closure = struct
  let make_iterator self =
    let n = A.length self in
    fun f empty i ->
      if i = n then empty
      else f (A.Unsafe.get i self) (i + 1)

  let rec find predicate iter index =
    iter
      (fun a index' ->
         if predicate a then Some a
         else find predicate iter index')
      None
      index

  let bench n a =
    assert (find (equal n) (make_iterator a) 0 = Some n)
end


module Proto_impl = struct
  let bench n a =
    assert (Proto.Data.Array.find (equal n) a = Some n)
end


let n = 250_000 - 1
let input = Array.init 1_000_000 (fun x -> x)

let () =
  let open Core_bench.Std in
  Printf.printf "Benchmark for Foldable.Base implementations (n = %d).\n" n;
  let bench n = Bench.Test.create ~name:n in
  Core.Command.run (Bench.make_command [
      bench "Baseline"                (fun () -> Baseline.bench n input);
      bench "Iter_mod"                (fun () -> Iter_mod.bench n input);
      bench "Foldk"                   (fun () -> Foldk.bench n input);
      bench "Iter_inspect_2"          (fun () -> Iter_inspect_2.bench n input);
      bench "Proto_impl"              (fun () -> Proto_impl.bench n input);
      (* bench "Iter_inspect_3"          (fun () -> Iter_inspect_3.bench n input); *)
      (* bench "Iter_inspect_1"          (fun () -> Iter_inspect_1.bench n input); *)
      (* bench "Iter_option"             (fun () -> Iter_option.bench n input); *)
      (* bench "Iter_k_stack_many"       (fun () -> Iter_k_stack_many.bench n input); *)
      (* bench "Iter_k_stack_tuple"   (fun () -> Iter_k_stack_tuple.bench n input); *)
      (* bench "Iter_k_stack_record"  (fun () -> Iter_k_stack_record.bench n input); *)
      (* bench "Iter_k_stack_closure" (fun () -> Iter_k_stack_closure.bench n input); *)
      (* bench "Iterable_array_closure"  (fun () -> Iterable_array_closure.bench n input); *)
    ])

