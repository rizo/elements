
open Base

type ('a, 'e) t = ('a, 'e) result

let (!) r =
  match r with
  | Ok x -> x
  | Error exn -> raise exn

let ok    x = Ok    x
let error x = Error x

let is_ok    = function (Ok _)    -> true | _ -> false
let is_error = function (Error _) -> true | _ -> false

let to_option = function (Ok x) -> Some x | Error _ -> None

let extract f = function
  | Ok a -> a
  | Error e -> f e

let map f = function
  | Ok a -> Ok (f a)
  | e -> e

let with_default default result =
  match result with
  | Ok value -> value
  | Error _ -> default

