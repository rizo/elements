
type ordering = LT | EQ | GT

let compare a b =
  match Pervasives.compare a b with
  | ord when ord > 0 -> GT
  | ord when ord < 0 -> LT
  | ord              -> EQ

type ('a, 'b) either =
  | Left  of 'a
  | Right of 'b

let ok x = Ok x
let error x = Error x


let either f g x =
  match x with
  | Left  l -> f l
  | Right r -> g r

let discard _ = ()

let undefined () = raise (Failure "Elements.undefined")

(* Printing and Formatting *)

let print = print_endline
let fmt = Printf.sprintf

(* Numeric Primitives *)

let even n = n mod 2 = 0
let odd  n = n mod 2 = 1

(* Channel *)

let output_line chan line =
  output_string chan (line ^ "\n")

(* Common signatures *)

module type Type = sig
  type t
end

module type Ord = sig
  type t
  val compare : t -> t -> ordering
end

module type Show = sig
  type t [@@deriving show]
end

module type Functor = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Id = struct
  module Make (X : Type) = X
  type 'a t = 'a
  let map f x = f x
end

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Log = struct
  let out level msg =
    output_line stderr (fmt "%s: %s"  level msg); flush stderr
  let info msg = out "info" msg
  let error msg = out "error" msg
  let warning msg = out "warning" msg
end

module type Default = sig
  type t
  val default : unit -> t
end

(* Exn base *)

let fail msg = raise (Failure msg)

let guard f x =
  try Some (f x)
  with _ -> None

(* Option base *)

let some x = Some x
let none   = None

let is_some = function Some _ -> true  | None -> false
let is_none = function Some _ -> false | None -> true

let option (default : 'b) (f : 'a -> 'b) (opt : 'a option) : 'b =
  match opt with
  | None -> default
  | Some x -> f x


(* Result base *)

let ok    x = Ok    x
let error x = Error x

let is_ok    = function Ok _ -> true  | Error _ -> false
let is_error = function Ok _ -> false | Error _ -> true

let result (default : 'b) (f : 'a -> 'b) (res : ('a, 'e) result) : 'b =
  match res with
  | Ok x    -> f x
  | Error _ -> default


(* Fn base *)
let id x        = x
let flip f x y  = f y x
let curry f x y = f (x, y)
let uncurry f p = f (fst p) (snd p)
let compose f g = fun x -> f (g x)
let (<<) f g    = compose f g
let (>>) g f    = compose f g

(* List base *)
let cons x xs = x::xs
let snoc xs x = x::xs

(* Iter base. *)
type ('a, 's) iter = 's * ('s -> ('a * 's) option)

