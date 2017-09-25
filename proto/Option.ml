
open Kernel
open Control

type 'a t = 'a option

exception No_value

let some x = Some x
let none   = None

let is_some = function Some _ -> true  | None -> false
let is_none = function Some _ -> false | None -> true

let is_empty = is_none

let option f default self =
  match self with
  | Some x -> f x
  | None   -> default ()

let or_else default self =
  match self with
  | Some x -> x
  | None   -> default ()

let ( or ) self default =
  match self with
  | Some x -> x
  | None   -> default

let force self =
  match self with
  | Some x -> x
  | None   -> raise No_value

let or_fail message self =
  match self with
  | Some x -> x
  | None -> fail message

let catch f =
  try
    Some (f ())
  with _ -> None


let each f self =
  match self with
  | Some x -> f x
  | None -> ()

(* Comparable1 *)
include Comparable1.Make(struct
    type nonrec 'a t = 'a t

    let compare cmp1 x y =
      match x, y with
      | None  , None   -> Comparable.equal
      | None  , Some _ -> Comparable.less
      | Some _, None   -> Comparable.greater
      | Some a, Some b -> cmp1 a b
  end)

(* Default1 *)
let default = None

(* Equatable1 *)
let equal eq1 x y =
  match x, y with
  | None  , None   -> true
  | Some a, Some b -> eq1 a b
  | _              -> false

(* Hashable1 *)
let hash _ self =
  Hashtbl.hash self

(* Printable1 *)
include Printable1.Make(struct
    type nonrec 'a t = 'a t

    let pp pp1 fmt self =
      match self with
      | None   ->
        Format.pp_print_string fmt "None"
      | Some x ->
        Format.pp_print_string fmt "(Some ";
        (pp1 fmt) x;
        Format.pp_print_string fmt ")"
  end)


(* Monad instance *)
module Monad_base = struct
  type nonrec 'a t = 'a t

  let return x = Some x

  let bind f opt =
    match opt with
    | Some x -> f x
    | None -> None
end

module Monad = Monad.Make(Monad_base)

(* Functor instance *)
(* FIXME: Only Monad.Base works here, not Monad *)
module Functor = Functor.With_monad(Monad_base)

include Monad
include Functor

