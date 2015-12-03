
open El_base

type ('a, 'e) t = ('a, 'e) result

let (!) r =
  match r with
  | Ok x -> x
  | Error exn -> raise exn

