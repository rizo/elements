
module L = Proto__List
module A = Proto__Array
let scoped = Proto.Control.scoped

let equal : int -> int -> bool =
  Pervasives.(=)


module Iteratees = struct
  type ('a, 'b) step =
    | Await of ('a option -> ('a, 'b) step)
    | Yield of 'b

  let rec enum_list xs step =
    match step with
    | Await k ->
        begin match xs with
        | [] -> k None
        | x :: xs' -> enum_list xs' (k (Some x))
        end
    | step -> step

  let find predicate =
    let rec go input =
      match input with
      | None -> Yield None
      | Some a when predicate a -> Yield (Some a)
      | Some _ -> Await go in
    Await go

  let run step =
    match step with
    | Await k -> raise (Failure "Not enough input")
    | Yield a -> a

  let bench n l =
    assert (run (enum_list l (find (equal n))) = Some n)
end


module Full = struct
  let product =
    let rec go r input =
      match input with
      | [] -> r
      | x :: xs -> go (r * x) xs in
    go 1
end

module Baseline = struct
  let product =
    let rec go r input =
      match input with
      | [] -> r
      | x :: xs ->
          if equal x 0 then 0
          else go (r * x) xs in
    go 1
end

module Uncons_fold = struct
  let uncons list =
    match list with
    | [] -> None
    | x :: xs -> Some (x, xs)

  let rec fold f r input =
    match uncons input with
    | None -> r
    | Some (x, xs) -> fold f (f x r) xs

  let rec find predicate input =
    match uncons input with
    | None -> None
    | Some (x, xs) ->
        if predicate x then Some x
        else find predicate xs

  let bench n l =
    assert (find (equal n) l = Some n)
end

module View_fold = struct
  let view f r self =
    match self with
    | [] -> r
    | x :: xs -> f x xs

  let product =
    let rec go r input =
      view
        (fun x xs -> if equal x 0 then 0
          else go (x * r) xs)
        r
        input in
    go 1
end

module Foldk = struct
  let foldk f =
    let rec go b self =
      match self with
      | [] -> b
      | a :: self' -> f a b (fun b' -> go b' self') in
    go

  let product =
    foldk
      (fun a b continue ->
        if a == 0 then 0
        else continue (a * b))
      1
end

(* This implementation has a huge heap allocation impact compared to Foldk. *)
module Foldk' = struct
  let rec foldk f r k self =
    match self with
    | [] -> k r
    | x :: xs -> f x r (fun r' -> foldk f r k xs)

  let find predicate input =
    foldk
      (fun a b continue ->
        if predicate a then Some a
        else continue b)
      None
      (fun x -> x)
      input

  let bench n l =
    assert (find (equal n) l = Some n)
end


module Fold_while = struct
  type 'a reduced = Continue of 'a | Stop of 'a

  let rec fold_while f b input =
    match input with
    | [] -> b
    | x :: xs ->
      begin match f x b with
        | Continue b' -> fold_while f b' xs
        | Stop b' -> b'
      end

  let find predicate input =
    fold_while
      (fun a b ->
        if predicate a then Stop (Some a)
        else Continue b)
      None
      input

  let bench n l =
    assert (find (equal n) l = Some n)
end

module Fold_break = struct
  let each f self = List.fold_left (fun () a -> f a) () self
  let find predicate self =
    scoped (fun break ->
      each (fun a -> if predicate a then break (Some a)) self;
      None)
end



let n = 500_000
let input = Array.(to_list (append (init n (fun x -> x + 1))
                                   (init (n - (n / 2)) (fun x -> x))))
let run product =
  assert (equal (product input) 0)


let () =
  let open Core_bench.Std in
  Printf.printf "Benchmark for Foldable.Base implementations (n = %d).\n" n;
  let bench n = Bench.Test.create ~name:n in
  Core.Command.run (Bench.make_command [
      (* bench "Full"                (fun () -> run Full.product); *)
      bench "Baseline"            (fun () -> run Baseline.product);
      bench "View_fold"           (fun () -> run View_fold.product);
      bench "Foldk"               (fun () -> run Foldk.product);
      bench "Proto"               (fun () -> run Proto.Data.List.product);
      (* bench "Foldk'"              (fun () -> Foldk'.bench n input); *)
      (* bench "Fold_break"          (fun () -> Fold_break.bench n input); *)
      (* bench "Uncons_fold"         (fun () -> Uncons_fold.bench n input); *)
      (* bench "Fold_while"          (fun () -> Fold_while.bench n input); *)
      (* bench "Fold_iteratee"       (fun () -> Iteratees.bench n input); *)
    ])

