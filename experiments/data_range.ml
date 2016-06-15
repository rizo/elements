

module Finite = struct
  module Forward = struct
    type 'a iter = Iter : ('s * ('s -> ('a * 's) option)) -> 'a iter

    module type Iterable = sig
      type 'a t
      val iter : 'a t -> 'a iter
    end

    module Make(I : Iterable) = struct
      let foldl f acc (Iter (s0, next)) =
        let rec loop acc s =
          match next s with
          | Some (a, s') -> loop (f acc a) s'
          | None         -> acc in
        loop acc s0

    end
  end
end


open Base

type range = Range of (int * int)

let init n m = Range (n, m)


module Iter : (Finite.Forward.Iterable with type 'a t = range) = struct
  type 'a t = range

  let iter : int t -> int Finite.Forward.iter= fun (Range (n, m)) ->
    let next (n, m) =
      if n = m then None
      else Some (n, (n + 1, m)) in
    Finite.Forward.Iter ((n, m), next)
end

include Finite.Forward.Make(Iter)

