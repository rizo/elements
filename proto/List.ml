
open Kernel
open Control
open Collection

type 'a t = 'a list


(* Monoid instance *)
include Monoid.Make(struct
    type nonrec 'a t = 'a t
    let empty = []

    let slow_append l1 l2 =
      Stdlib.List.rev_append (Stdlib.List.rev l1) l2

    let rec count_append l1 l2 count =
      match l2 with
      | [] -> l1
      | _ ->
          match l1 with
          | []               ->                         l2
          | [x1]             -> x1                   :: l2
          | [x1; x2]         -> x1 :: x2             :: l2
          | [x1; x2; x3]     -> x1 :: x2 :: x3       :: l2
          | [x1; x2; x3; x4] -> x1 :: x2 :: x3 :: x4 :: l2
          | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
              x1 :: x2 :: x3 :: x4 :: x5 ::
              (if count > 1000
               then slow_append  tl l2
               else count_append tl l2 (count + 1))

    let append self other =
      count_append self other 0
  end)


(* Default1 instance *)
let default = empty


(* Comparable instance *)
include Comparable1.Make(struct
    type nonrec 'a t = 'a t

    let compare cmp a b =
      let rec loop a b =
        match a, b with
        | [], [] -> Comparable.equal
        | [], _  -> Comparable.less
        | _ , [] -> Comparable.greater
        | x :: xs, y :: ys ->
            let n = cmp x y in
            if n = Comparable.equal
            then loop xs ys
            else n in
      loop a b
  end)


(* Functor instance *)
include Functor.Make(struct
    type nonrec 'a t = 'a t

    (* Based on Petter A. Urkedal's implementation
       Link: https://discuss.ocaml.org/t/a-new-list-map-that-is-both-stack-safe-and-fast/865/10 *)
    let map f xs =
      let rec rise ys = function
      | [] -> ys
      | (y0, y1, y2, y3, y4, y5, y6, y7) :: bs ->
          rise (y0 :: y1 :: y2 :: y3 :: y4 :: y5 :: y6 :: y7 :: ys) bs in
      let rec dive bs = function
      | x0 :: x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: xs ->
          dive ((f x0, f x1, f x2, f x3, f x4, f x5, f x6, f x7) :: bs) xs
      | xs -> rise (Stdlib.List.map f xs) bs in
      dive [] xs
  end)


(* Iterable & Container instance *)
module Basic_iterable = struct
  type nonrec 'a t = 'a t
  type 'a state = 'a t

  let init self = self

  let next self s f r =
    match s with
    | [] -> r
    | a :: s' -> f a s'
end
include Iterable.Make(Basic_iterable)
include Container.With_iterable(Basic_iterable)


(* FIXME: -n *)
let make n f =
  let rec loop i acc =
    if i < 0 then acc
    else loop (i - 1) (f i :: acc) in
  loop (n - 1) []


let foldk f =
  let rec go b self =
    match self with
    | [] -> b
    | a :: self' -> f a b (fun b' -> go b' self') in
  go


let add x xs = x :: xs

let inspect f r self =
  match self with
  | [] -> r
  | x :: xs -> f x xs

let head self =
  match self with
  | [] -> None
  | x :: _ -> Some x

let tail self =
  match self with
  | [] -> None
  | _ :: xs -> Some xs


(* {e Complexity:} O(2n), if [n] is negative. *)
(* let get n self = *)
(*   let rec loop i n self' = *)
(*     match self' with *)
(*     | [] -> None *)
(*     | x :: xs when i = n -> Some x *)
(*     | _ :: xs -> loop (i + 1) n xs in *)
(*   if n < 0 then *)
(*     loop 0 (Int.abs n - 1) (reverse self) *)
(*   else loop 0 n self *)


let rec fold_right f acc self =
  match self with
  | [] -> acc
  | x :: xs -> f x (fold_right f acc xs)

let reverse self =
  Stdlib.List.fold_left (fun xs x -> x :: xs) [] self


let reverse_indexed ?from:(i = 0) self =
  let rec loop i acc self' =
    match self with
    | [] -> acc
    | x :: xs -> loop (i + 1) ((i, x) :: acc) xs in
  loop i [] self

let indexed ?from:(i = 0) self =
  reverse (reverse_indexed ~from:i self)

let concat l = fold_right append [] l

let chunks size self =
  if size <= 0 then
    raise (Invalid_argument "chunk size <= 0")
  else
    let rec loop i chunk res self' =
      match self' with
      | [] when chunk = [] -> res
      | [] -> reverse chunk :: res
      | x :: xs when i = size -> loop 1 [x] (reverse chunk :: res) xs
      | x :: xs -> loop (i + 1) (x :: chunk) res xs in
    reverse (loop 0 [] [] self)

let of_array = Stdlib.Array.to_list

let pairwise self =
  match self with
  | [] -> []
  | [_] -> raise (Invalid_argument "singleton list")
  | x :: xs ->
    let rec loop r self' =
      match self' with
      | [] -> r
      | [x] -> r
      | x0 :: (x1 :: _ as xs) ->
        loop ((x0, x1) :: r) xs in
    loop [] self


(* Processing functions *)

let reverse_take n self =
  if n < 0 then
    raise (Invalid_argument "negative slice size")
  else
  if n = 0 || self = [] then
    []
  else
  let rec loop i n r self' =
    match self' with
    | [] -> r
    | x :: xs when i = n -> r
    | x :: xs -> loop (i + 1) n (x :: r) xs in
  loop 0 n [] self

let take n self =
  reverse (reverse_take n self)


module Unsafe = struct
  let head self =
    match self with
    | [] -> raise (Failure "called head on empty list")
    | x :: _ -> x

  let tail self =
    match self with
    | [] -> raise (Failure "called tail on empty list")
    | _ :: xs -> xs

  let get n self =
    let rec loop i self' =
      match self' with
      | [] ->
        raise (Failure (format "index %d out of range, list length = %d" n (length self)))
      | x :: xs when i = n -> x
      | _ :: xs -> loop (i + 1) xs in
    if n < 0 then
      raise (Invalid_argument (format "negative index %d" n))
    else loop 0 self
end

