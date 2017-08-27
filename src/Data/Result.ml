
type ('a, 'e) t = ('a, 'e) result

exception Not_ok

let ok     x = Ok x
let error  e = Error e

let is_ok self =
  match self with
  | Ok _    -> true
  | Error _ -> false

let is_error self =
  match self with
  | Ok _    -> false
  | Error _ -> true

let force self =
  match self with
  | Ok x    -> x
  | Error e -> raise Not_ok

let catch f =
  try
    Ok (f ())
  with e ->
    Error e

let or_else default self =
  match self with
  | Ok x    -> x
  | Error _ -> default ()

let ( or ) self default =
  match self with
  | Ok x    -> x
  | Error _ -> default

let result f default self =
  match self with
  | Ok x    -> f x
  | Error e -> default e

let to_option self =
  match self with
  | Ok x    -> Some x
  | Error _ -> None

let of_option opt err =
  match opt with
  | Some x  -> Ok x
  | None    -> Error err

(* Comparable2 *)
include Comparable2.Make(struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let compare cmp1 cmp2 x y =
      match x, y with
      | Error e1, Error e2 -> cmp2 e1 e2
      | Error _ , Ok    _  -> Comparable.less
      | Ok _    , Error _  -> Comparable.greater
      | Ok x1   , Ok x2    -> cmp1 x1 x2
  end)

(* Equatable2 *)
let equal eq1 eq2 x y =
  match x, y with
  | Ok x1   , Ok x2    -> eq1 x1 x2
  | Error e1, Error e2 -> eq2 e1 e2
  | _                  -> false

(* Hashable2 *)
let hash _ _ self =
  Hashtbl.hash self

(* Printable2 *)
include Printable2.Make(struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let pp pp1 pp2 fmt self =
      match self with
      | Error e ->
        Format.pp_print_string fmt "(Error ";
        (pp2 fmt) e;
        Format.pp_print_string fmt ")"
      | Ok x ->
        Format.pp_print_string fmt "(Ok ";
        (pp1 fmt) x;
        Format.pp_print_string fmt ")"
  end)


module Monad_base = struct
  type nonrec ('a, 'e) t = ('a, 'e) result

  let return x = Ok x

  let bind f res =
    match res with
    | Ok x -> f x
    | Error e -> Error e
end

module Monad = Monad2.Make(Monad_base)
module Functor = Functor2.With_monad(Monad_base)

include Monad
include Functor

