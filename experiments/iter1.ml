
type 'a iter = Iter : 's * ('s -> ('a * 's) option) -> 'a iter

module Iter = struct
  type 'a t = 'a iter

  module type Input = sig
    type 'a t
    val iterate : 'a t -> 'a iter
  end

  module type Output = sig
    type 'a t
    val collect : 'a iter -> 'a t
  end

  let map f (Iter (s0, next)) =
    let rec next' s =
      match next s with
      | Some (a, s') -> Some (f a, s')
      | None -> None in
    Iter (s0, next')

  let filter p (Iter (s0, next)) =
    let rec next' s =
      match next s with
      | Some (a, s') ->
        if p a then Some (a, s')
        else next s'
      | None -> None in
    Iter (s0, next')

  let take n (Iter (s0, next)) =
    let next' (s, i) =
      if i <= 0 then None
      else match next s with
        | Some (a, s') -> Some (a, (s', i - 1))
        | None -> None in
    Iter ((s0, n), next')
end

module List = struct
  include List

  let iterate self =
    let next s =
      match s with
      | [] -> None
      | x :: s' -> Some (x, s') in
    Iter (self, next)

  let collect (Iter (s0, next)) =
    let rec loop acc s =
      match next s with
      | None -> List.rev acc
      | Some (x, s') -> loop (x :: acc) s' in
    loop [] s0
end

let fibonacci () =
  let next (n, m) = Some (n, (m, n + m)) in
  Iter ((0, 1), next)


let () = begin
  let l1 = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let l2 = l1
           |> List.iterate
           |> List.collect  in
  assert (l1 = l2);

  let l3 = l1
           |> List.iterate
           |> Iter.filter (fun a -> a mod 2 = 0)
           |> List.collect in
  assert (l3 = [0; 2; 4; 6; 8]);

  let l4 = fibonacci ()
           |> Iter.take 10
           |> List.collect in
  assert (l4 = [1; 1; 2; 3; 5; 8; 13; 21; 34]);

  let l5 = fibonacci ()
           |> Iter.take 10
           |> Iter.map string_of_int
           |> List.collect in
  assert (l5 = ["1"; "1"; "2"; "3"; "5"; "8"; "13"; "21"; "34"])
end


