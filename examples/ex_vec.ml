
open Elements

type 'a iter = Iter : 's -> ('a * 's) iter

module type Forward = sig
  type 'a t
  val iterate : 'a t -> 'a iter
end

let v1 = vec [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]

let v2 =
  Vec.iter v1
  |> Iter.map (fun x -> x + 100)
  |> Iter.filter even
  |> Vec.collect()


