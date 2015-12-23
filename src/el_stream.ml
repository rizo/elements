
open El_base

type 'a t =
  | Empty
  | Yield of 'a * 'a t lazy_t

let empty = Empty

let yield a = Yield (a, lazy empty)

let next s =
  match s with
  | Empty -> None
  | Yield (x, lazy s) -> Some (x, s)

let rec append s1 s2 =
  match s1 with
  | Empty -> Lazy.force s2
  | Yield (x, lazy s') -> Yield (x, lazy (append s' s2))

let (++) = append

let rec count n =
  yield n ++ lazy (count (n + 1))

let rec cycle g =
  g ++ lazy (cycle g)

let rec take n s =
  if n = 0 then empty
  else match next s with
    | Some (x, s) -> yield x ++ lazy (take (n - 1) s)
    | None        -> empty

let rec each f g =
  match g with
  | Yield (x, lazy g) -> f x; each f g
  | Empty             -> ()

let rec map f g =
  match g with
  | Yield (x, lazy g) -> yield (f x) ++ lazy (map f g)
  | Empty             -> empty

let rec filter p g =
  match g with
  | Yield (x, lazy g) -> if p x then yield x ++ lazy (filter p g) else filter p g
  | Empty             -> empty


let fold ~f ~init s =
  let rec loop s acc =
    match s with
    | Yield (a, lazy s) -> loop s (f acc a)
    | Empty             -> acc in
  loop s init

let length g =
  fold ~init:0 ~f:(fun c _ -> c + 1) g

(* Lists *)
let l0 = []
let l1 = [1; 2; 3; 4; 5; 6; 7]
let rec l2 = 0::l2

let rec of_list xs =
  match xs with
  | []      -> empty
  | x :: xs -> Yield (x, lazy (of_list xs))

let to_list g =
  let rec loop acc g =
    match g with
    | Yield (a, lazy g) -> loop (a::acc) g
    | Empty            -> List.rev acc
  in loop [] g

let of_array a =
  let rec loop i =
    if i = 0 then empty
    else (yield a.(i) ++ lazy (loop (i - 1))) in
  loop (Array.length a - 1)

let to_array g =
  let curr = ref g in
  Array.init (length g) (fun _ ->
      match !curr with
      | Yield (x, lazy g) -> curr := g; x
      | Empty             -> assert false)

let of_channel c =
  let rec loop () =
    try (yield (input_line c) ++ lazy (loop ()))
    with End_of_file -> empty in
  loop ()

let to_channel c g =
  each (fun line -> output_line c line) g

module type Enum = sig
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
  val filter : ('a -> bool) -> 'a t -> 't
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val find : ('a -> bool) -> 'a t -> 'a t
  val find_index : ('a -> bool) -> 'a list -> int option
  val find_indices : ('a -> bool) -> 'a list -> int list
  val flat_map : ('a -> 'b list) -> 'a list -> 'b list
  val fold_left : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r
  val fold_right : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r
  val fold_until : ('r -> 'a -> ('r, 'r) either) -> 'r -> 'a t -> 'r
  val group : ?by: ('a -> 'a -> bool) -> 'a t -> 'a t t
  val head : 'a t -> 'a option
  val indexed : 'a t -> (int * 'a) t
  val intersperse : 'a -> 'a t -> 'a t
  val iterate : ('a -> 'a) -> 'a -> 'a t
  val join : ('a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val max : ?cmp: ('a -> 'a -> ordering) -> 'a t -> 'a
  val min : ?cmp: ('a -> 'a -> ordering) -> 'a t -> 'a
  val nth : int -> 'a t -> 'a option
  val partition : ('a -> bool) -> 'a t -> ('a t * 'a t)
  val reduce : ('r -> 'a -> 'r) -> 'a t -> 'r option
  val reject : ('a -> bool) -> 'a t -> 't
  val repeat : 'a -> 'a t
  val replicate : int -> 'a -> 'a t
  val reverse : 'a t -> 'a t
  val scanl : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r t
  val scanr : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r t
  val slice : int -> int -> 'a t -> 'a t
  val sort : ?by:('a -> 'a -> ordering) -> ?on: ('a -> 'b) -> 'a t -> 'a t
  val split : at: int -> 'a t -> ('a t * 'a t)
  val split_while : ('a -> bool) -> 'a t -> ('a t * 'a t)
  val take : int -> 'a t -> 'a t
  val take_every : int -> 'a t -> 'a t
  val take_while : ('a -> bool) -> 'a t -> 'a t
  val uniq : ('a -> 'a -> bool) -> 'a t -> 'a t
  val zip : 'a t -> 'b t -> ('a * 'b) t
end

