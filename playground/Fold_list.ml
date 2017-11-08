open Proto

  type 'a seq = { run : 'r . ('a -> 'r -> 'r) -> 'r -> 'r } [@@unboxed]

let filter p seq =
  let run f = seq.run (fun a r -> if p a then f a r else r) in
  { run }

let map f seq =
  let run g = seq.run (g << f) in
  { run }

let take n seq =
  let run f z = seq.run (fun a r n -> if n <= 0 then z else f a (r (n - 1))) (always z) n in
  { run }

let rec fold f r list =
  match list with
  | [] -> r
  | a :: rest -> print "."; fold f (f a r) rest

let to_list seq =
  seq.run (fun a r -> a :: r) []

let of_list list = { run = fun f r -> fold f r list }

let take_while p seq =
  let run f z = seq.run (fun a r -> if p a then f a r else z) z in
  { run }

