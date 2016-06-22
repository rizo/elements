

module Forward = struct
  type 'a iter = Iter : ('s * ('s -> ('a * 's) option)) -> 'a iter

  module type Iterable = sig
    type 'a t
    val iter : 'a t -> 'a iter
  end

  module type Sig = sig
    type 'a t
    val foldl : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r
  end

  module Make(I : Iterable) : (Sig with type 'a t = 'a I.t) = struct
    type 'a t = 'a I.t

    let foldl f acc iterable =
      let Iter (s0, next) = I.iter iterable in
      let rec loop acc s =
        match next s with
        | Some (a, s') -> loop (f acc a) s'
        | None         -> acc in
      loop acc s0
  end
end

module List
  : sig
    type 'a t
    include Forward.Sig with type 'a t := 'a t
  end
= struct
  type 'a t = 'a list

  module Iterable : (Forward.Iterable with type 'a t = 'a t) = struct
    type nonrec 'a t = 'a t

    let iter self =
      let next self =
        match self with
        | a :: rest -> Some (a, rest)
        | []        -> None in
      Forward.Iter (self, next)
  end

  module Iterator : Forward.Sig with type 'a t = 'a Iterable.t = Forward.Make(Iterable)
  include Iterator
end

(* module Range = struct *)
  (* type 'a t = Range of (int * int) *)

  (* module Iterable : (Forward.Iterable with type 'a t = int t) = struct *)
    (* type nonrec 'a t = int t *)

    (* let iter ((Range (n, m)) : int t) : int Forward.iter = *)
      (* let next (n, m) = *)
        (* if n = m then None *)
        (* else Some (n, (n + 1, m)) in *)
      (* Forward.Iter ((n, m), next) *)
  (* end *)

  (* include Forward.Make(Iterable) *)
(* end *)


