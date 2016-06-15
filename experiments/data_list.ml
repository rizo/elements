



module Finite = struct
  module Forward = struct
    type 'a iter = Iter : ('s * ('s -> ('a * 's) option)) -> 'a iter

    module type Iterable = sig
      type 'a t
      val iter : 'a t -> 'a iter
    end

    module Make(I : Iterable) = struct

      let foldl f acc iterable =
        let rec loop acc s =
          match next s with
          | Some (a, s') -> loop (f acc a) s'
          | None         -> acc in
        loop acc s0

    end
  end
end


module List = struct
  type 'a t = 'a list

  module Iter : (Finite.Forward.Iterable with type 'a t = 'a list) = struct
    type 'a t = 'a list
    let iter self =
      let next self =
        match self with
        | a :: rest -> Some (a, rest)
        | []        -> None in
      Finite.Forward.Iter (self, next)
  end

  include Finite.Forward.Make(Iter)
end



