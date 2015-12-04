
open El_base

type ('a, 'e) t = ('a, 'e) result

let (!) r =
  match r with
  | Ok x -> x
  | Error exn -> raise exn

let ok    x = Ok    x
let error x = Error x

let is_ok    = function (Ok _)    -> true | _ -> false
let is_error = function (Error _) -> true | _ -> false

