
(* Kernel *)
let pass () = ()

let bracket init (free : 'a -> unit) f =
  let x = init () in
  try
    let r = f x in
    free x;
    r
  with exn ->
    free x;
    raise exn

(* End Kernel *)

module Public = struct
  type 'a iter =
    Iter : {
      init : unit -> 'cursor;
      next : 'r . ('a -> 'cursor -> 'r) -> 'r -> 'cursor -> 'r;
      stop : 'cursor -> unit;
    } -> 'a iter

  type ('a, 'r) reducer =
    Reducer : {
      init : unit -> 'accumulator;
    reduce : 'a -> 'accumulator -> 'accumulator;
    extract : 'accumulator -> 'r;
    } -> ('a, 'r) reducer
end

include Public

type 'a t = 'a iter


(* Producers *)

let empty =
  Iter { init = pass;
         next = (fun _ r _ -> r);
         stop = pass }

let make n f =
  (* FIXME: Add checks *)
  let next yield r s =
    if s = n then r
    else yield (f s) (s + 1) in
  Iter { init = (fun () -> 0);
         next;
         stop = ignore }

let count ?by:(step = 1) start =
  (* FIXME: Add checks *)
  let next yield _r n = yield n (n + step) in
  Iter { init = (fun () -> start);
         next;
         stop = ignore }

let range ?by:(step = 1) start stop =
  (* FIXME: Add checks *)
  let next yield r s =
    if s >= stop then r
    else yield s (s + step) in
  Iter { init = (fun () -> start); next; stop = ignore }

let iota ?by stop =
  range ?by 0 stop

let repeat x =
  Iter { init = pass;
         next = (fun yield _ -> yield x);
         stop = pass }

let repeatedly f =
  Iter { init = pass;
         next = (fun yield _ -> yield (f ()));
         stop = pass }

let iterate f x =
  Iter { init = (fun () -> x);
         next = (fun yield _ s -> yield s (f s));
         stop = ignore }

let of_list list =
  let next yield r s =
    match s with
    | [] -> r
    | a :: s' -> yield a s' in
  Iter { init = (fun () -> list); next; stop = ignore; }

let of_array array =
  let next yield r idx =
    if Array.length array = idx then r
    else yield (Array.unsafe_get array idx) (idx + 1) in
  Iter { init = (fun () -> 0); next; stop = ignore }


(* Transformers *)

let select predicate (Iter iter) =
  let[@inline] rec next yield r s =
    iter.next
      ((fun a s' ->
          if predicate a then yield a s'
          else next yield r s')) r s in
  Iter { iter with next }

let filter = select
let reject predicate = select (fun x -> not (predicate x))

let map f (Iter iter) =
  let[@inline] next yield r s =
    iter.next (fun a s' -> yield (f a) s') r s in
  Iter { iter with next }

let take n (Iter iter) =
  if n = 0 then empty else
  if n < 0 then raise (Invalid_argument ("take " ^ string_of_int n)) else
  let[@inline] next yield r (s, i) =
    if i <= 0 then r
    else iter.next (fun a s' -> yield a (s', i - 1)) r s in
  Iter { init = (fun () -> (iter.init (), n));
         stop = (fun (s, _) -> iter.stop s);
         next }


(* Consumers *)

let each (f : 'a -> unit) (Iter iter) =
  let rec loop s =
    iter.next (fun a s' -> f a; loop s') () s in
  bracket iter.init iter.stop loop

let[@inline] fold f r0 (Iter iter) =
  let[@inline] rec loop r s =
    iter.next (fun a s' -> loop (f a r) s') r s in
  bracket iter.init iter.stop (loop r0)

let reduce f (Iter iter) =
  let[@inline] rec loop r s =
    iter.next (fun a s' -> loop (f a r) s') r s in
  bracket iter.init iter.stop
    (iter.next
       (fun r0 s0 -> Some (loop r0 s0))
       None)

let fold_while f r0 (Iter iter) =
  let rec loop r s =
    iter.next
      (fun a s' ->
         match f a r with
         | `Continue r' -> loop r' s'
         | `Stop r' -> r')
      r s in
  bracket iter.init iter.stop (loop r0)

let find predicate (Iter iter) =
  let rec loop s =
    iter.next
      (fun a s' ->
         if predicate a then Some a
         else loop s')
      None s in
  bracket iter.init iter.stop loop

let find_index predicate (Iter iter) =
  let rec loop idx s =
    iter.next
      (fun a s' ->
         if predicate a then Some idx
         else loop (idx + 1) s')
      None s in
  bracket iter.init iter.stop (loop 0)

let find_indices predicate (Iter iter) =
  let rec loop idx r s =
    iter.next
      (fun a s' ->
         if predicate a then loop (idx + 1) (idx :: r) s'
         else loop (idx + 1) r s')
      r s in
  loop 0 []
  |> bracket iter.init iter.stop
  |> List.rev

let index ?equal x self =
  let equal = match equal with Some f -> f | None -> (=) in
  find_index (equal x) self

let indices ?equal x self =
  let equal = match equal with Some f -> f | None -> (=) in
  find_indices (equal x) self

let find_max ?by self =
  let compare = match by with Some f -> f | None -> Pervasives.compare in
  let (>) a b = compare a b = 1 in
  reduce (fun a b -> if a > b then a else b) self

let find_min ?by self =
  let compare = match by with Some f -> f | None -> Pervasives.compare in
  let (<) a b = compare a b = -1 in
  reduce (fun a b -> if a < b then a else b) self

let contains x self =
  match find ((=) x) self with
  | Some _ -> true
  | _ -> false

let count predicate (Iter iter) =
  let rec loop n s =
    iter.next
      (fun a s' ->
         if predicate a then loop (n + 1) s'
         else loop n s')
      n s in
  bracket iter.init iter.stop (loop 0)

let sum self =
  fold ( + ) 0 self

let product (Iter iter) =
  let rec loop r s =
    iter.next
      (fun a s' ->
         if a = 0 then 0
         else loop (a * r) s')
      r s in
  bracket iter.init iter.stop (loop 1)

let all p (Iter iter) =
  let rec loop s =
    iter.next
      (fun a s' ->
         if p a then loop s'
         else false)
      true s in
  bracket iter.init iter.stop loop

let any p (Iter iter) =
  let rec loop s =
    iter.next
      (fun a s' ->
         if p a then loop s'
         else true)
      false s in
  bracket iter.init iter.stop loop

let to_list_reversed self =
  fold (fun x xs -> x :: xs) [] self

let to_list self =
  List.rev (to_list_reversed self)

let is_empty (Iter iter) =
  bracket iter.init iter.stop (iter.next (fun _ _ -> false) true)

let length self =
  fold (fun _ n -> n + 1) 0 self

let get n (Iter iter) =
  let rec loop idx s =
    iter.next
      (fun a s' ->
         if idx = n then Some a
         else loop (idx + 1) s')
      None s in
  bracket iter.init iter.stop (loop 0)

let first self  = get 0 self
let second self = get 1 self

let last self =
  fold (fun a _ -> Some a) None self

let rest (Iter iter) =
  bracket iter.init iter.stop
    (iter.next
       (fun _a s' -> Some (Iter { iter with init = (fun () -> s') }))
       None)

let into (Reducer f) (Iter i) =
  let rec loop r s =
    i.next (fun a s' -> loop (f.reduce a r) s') r s in
  let r0 = f.init () in
  try
    f.extract (bracket i.init i.stop (loop r0))
  with exn ->
    let _r = f.extract r0 in
    raise exn


