
type ('a, 's, 'r) iter =
  < init : 's; next : ('a -> 's -> 'r) -> 'r -> 's -> 'r >

let each f (self : ('a, 's, unit) iter) =
  let rec go s =
    self#next (fun a s' -> f a; go s') () s in
  go self#init

module Iterable1 = struct
  module type Basic = sig
    type 'a t
    type 'a state

    val init : 'a t -> 'a state
    val next : ('a -> 'a state -> 'r) -> 'r -> 'a state -> 'a t -> 'r
  end
  module Make (B : Basic) = struct
    let each f self = each f
        (object
          method init = B.init self
          method next f e s = B.next f e s self
        end)
  end
end

module Iterable_list = Iterable1.Make(struct
    type nonrec 'a t = 'a list
    type 'a state = 'a list

    let init self = self

    let next f r s self =
      match s with
      | [] -> r
      | a :: s' -> f a s'
  end)

