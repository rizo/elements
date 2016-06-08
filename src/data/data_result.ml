
open Base

(* Base definitions *)

type ('a, 'e) t = ('a, 'e) Base.result

let ok    x  = Base.ok
let error x  = Base.error
let is_ok    = Base.is_ok
let is_error = Base.is_error
let result   = Base.result


(* Value extraction *)

(** Examples:

    assert (force (Int.read "2") + 2 = 4)
 *)
let force ?(msg = "Result.force") self =
  match self with
  | Ok value -> value
  | Error _  -> raise (Failure msg)


(** Examples:

    assert (Result.safe 0 (Int.read "x") + 1 = 1)
 *)
let safe default self =
  match self with
  | Ok value -> value
  | Error _  -> default


(** Examples:

    assert (catch (Int.read "x")
                  (fun e -> Debug.log e; 0)
             = 0)
 *)
let catch f self =
  match self with
  | Ok value -> value
  | error    -> f error


(* Functor instance *)

let map f self =
  match self with
  | Ok a  -> Ok (f a)
  | error -> error


(* Conversion *)

let to_option = function Ok x -> Some x | Error _ -> None

