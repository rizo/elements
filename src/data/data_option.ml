
type 'a t = 'a option

(* Base definitions *)

let some    = Base.some
let none    = Base.none
let is_some = Base.is_some
let is_none = Base.is_none
let option  = Base.option


(* Value extraction *)

(** Examples:

    assert (force (List.head [1; 2; 3]) = 1)
 *)
let force self =
  match self with
  | Some x -> x
  | None -> raise (Failure "Option.force")


(** Examples:

    assert (safe 0 (List.head []) + 1 = 1)
 *)
let safe default self =
  match self with
  | Some x -> x
  | None -> default


(** Examples:
    assert (catch (List.head [])
                  (fun e -> Debug.log e; 0)
             = 0)
*)
let catch self f =
  match self with
  | Some x -> x
  | None -> f ()


(* Functor instance *)

let map f self =
  match self with
  | Some x -> Some (f x)
  | none   -> None

(* Monad instance *)

let return x = Some x

let (>>=) opt f =
  match opt with
  | Some x -> f x
  | none -> None

let (>>) opt1 opt2 =
  opt1 >>= fun _ -> opt2

