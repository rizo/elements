module Stdlib = Proto_shadow_stdlib

type 'a t = Empty | Yield of 'a * (unit -> 'a t)

let empty = Empty

let yield a = Yield (a, fun () -> empty)

let next s =
  match s with
  | Empty -> None
  | Yield (x, s) -> Some (x, s ())

let rec append s1 s2 =
  match s1 with
  | Empty -> s2 ()
  | Yield (x, s') -> Yield (x, fun () -> append (s' ()) s2)

let (++) = append

let rec count n =
  yield n ++ fun () -> count (n + 1)

let rec cycle s () =
  s ++ cycle s

let rec take n s =
  if n = 0 then empty
  else match next s with
    | Some (x, s) -> yield x ++ fun () -> take (n - 1) s
    | None        -> empty

let rec each f s =
  match s with
  | Yield (x, s') -> f x; each f (s' ())
  | Empty          -> ()

let rec map f s =
  match next s with
  | Some (x, s') -> yield (f x) ++ fun () -> map f s'
  | None         -> empty

let rec filter f s =
  match next s with
  | Some (x, s') when f x -> yield x ++ fun () -> filter f s'
  | Some (x, s')          -> filter f s'
  | None                  -> empty

let fold f init s =
  let rec loop acc s =
    match s with
    | Yield (a, s') -> loop (f acc a) (s' ())
    | Empty          -> acc in
  loop init s

let length g = fold (fun c _ -> c + 1) 0 g

let rec range start ?by:(step = 1) stop =
  if start >= stop
    then Empty
    else Yield (start, fun () -> range (start + step) stop ~by:step)

let rec drop n s =
  match s with
  | s when n = 0 -> s
  | Empty -> Empty
  | Yield (_, s') -> drop (n - 1) (s' ())

let rec of_list xs =
  match xs with
  | []      -> empty
  | x :: xs -> Yield (x, fun () -> of_list xs)

let to_list s =
  let rec loop acc s =
    match s with
    | Yield (a, s') -> loop (a::acc) (s' ())
    | Empty          -> Stdlib.List.rev acc
  in loop [] s

let of_array a =
  let rec loop i =
    if i = 0 then empty
    else (yield (Stdlib.Array.get a i) ++ fun () -> loop (i - 1)) in
  loop (Stdlib.Array.length a - 1)

let to_array s =
  let curr = ref s in
  Array.make (length s) (fun _ ->
      match !curr with
      | Yield (x, s') -> curr := s' (); x
      | Empty          -> assert false)

let of_channel c =
  let rec loop () =
    try (yield (input_line c) ++ fun () -> loop ())
    with End_of_file -> empty in
  loop ()

let to_channel c s =
  each (fun line -> output_string c (line ^ "\n")) s

(* Combinators *)

(* val or : bool list -> bool *)
let or' s = fold (fun r a -> r || a) false s

(* val any : ('a -> bool) -> 'a t -> bool *)
let any f s = or' (map f s)


module type S = sig
  type 'a t
  val all : ('a -> bool) -> 'a t -> bool
  val any : ('a -> bool) -> 'a t -> bool
  val append : 'a t -> 'a t -> 'a t
  val count : 'a t -> int
  val cycle : 'a t -> 'a t
  val delete : 'a -> 'a t -> 'a t
  val drop : int -> 'a t -> 'a t
  val drop_while : ('a -> bool) -> 'a t -> 'a t
  val each : ('a -> unit) -> 'a t -> unit
  val elem : 'a -> 'a t -> bool
  val elem_index : 'a -> 'a t -> int option
  val elem_indices : 'a -> 'a t -> int list
  val empty : 'a t -> bool
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val find : ('a -> bool) -> 'a t -> 'a t
  val find_index : ('a -> bool) -> 'a list -> int option
  val find_indices : ('a -> bool) -> 'a list -> int list
  val find_max : ?by: ('a -> 'a -> int) -> 'a t -> 'a
  val find_min : ?by: ('a -> 'a -> int) -> 'a t -> 'a
  val flat_map : ('a -> 'b list) -> 'a list -> 'b list
  val fold_left : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r
  val fold_right : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r
  val fold_while : ('r -> 'a -> [ `Continue of 'r | `Break of 'r ]) -> 'r -> 'a t -> 'r
  val group : ?by: ('a -> 'a -> bool) -> 'a t -> 'a t t
  val head : 'a t -> 'a option
  val indexed : 'a t -> (int * 'a) t
  val intersperse : 'a -> 'a t -> 'a t
  val iterate : ('a -> 'a) -> 'a -> 'a t
  val join : ('a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val nth : int -> 'a t -> 'a option
  val bool_or : bool t -> bool
  val bool_and : bool t -> bool
  val partition : ('a -> bool) -> 'a t -> ('a t * 'a t)
  val reduce : ('r -> 'a -> 'r) -> 'a t -> 'r option
  val reject : ('a -> bool) -> 'a t -> 't
  val repeat : 'a -> 'a t
  val replicate : int -> 'a -> 'a t
  val reverse : 'a t -> 'a t
  val scanl : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r t
  val scanr : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r t
  val slice : int -> int -> 'a t -> 'a t
  val sort : ?by:('a -> 'a -> int) -> ?on: ('a -> 'b) -> 'a t -> 'a t
  val split : at: int -> 'a t -> ('a t * 'a t)
  val split_while : ('a -> bool) -> 'a t -> ('a t * 'a t)
  val take : int -> 'a t -> 'a t
  val take_every : int -> 'a t -> 'a t
  val take_while : ('a -> bool) -> 'a t -> 'a t
  val uniq : ('a -> 'a -> bool) -> 'a t -> 'a t
  val zip : 'a t -> 'b t -> ('a * 'b) t
end

