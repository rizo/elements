
type 'a t =
  | Node of 'a list * 'a * 'a list
  | Nil

let empty = Nil

let of_list l =
  match l with
  | x :: xs -> Node ([], x, xs)
  | [] -> Nil

let current = function
  | Node (_, x, _) -> Some item
  | Nil -> None

let next = function
  | Node (prev, a, b :: next) ->
      Node (a :: prev, b, next)
  | Nil -> Nil

let prev = function
  | Node (a::prev, b, next) ->
      Node (prev, a, b::next)
  | Nil -> Nil

